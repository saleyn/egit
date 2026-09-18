//-----------------------------------------------------------------------------
// libgit2 credential callback support - handles both direct credentials
// and dynamic MFA callbacks from Erlang
//-----------------------------------------------------------------------------
#pragma once

#include <git2/credential.h>
#include <thread>
#include <mutex>
#include <map>
#include <memory>
#include <cstdlib>
#include <sys/stat.h>
#include <erl_nif.h>

namespace {
  // Thread-local credential context storage
  thread_local ERL_NIF_TERM g_credentials_source = 0;
  thread_local ErlNifEnv* g_callback_env = nullptr;

  // Guards against infinite retry loops: libgit2 will keep calling the
  // credential callback as long as we keep returning "new" credentials
  // that then fail authentication (e.g. no usable key in the ssh-agent).
  // Without a cap, a request against a host requiring real credentials
  // we don't have can spin forever re-trying the same default guess.
  thread_local int g_credential_attempts = 0;
  constexpr int MAX_CREDENTIAL_ATTEMPTS = 3;

  std::mutex g_credentials_mutex;

  /**
   * Thread-local credential context - stores credentials info needed by callback
   */
  struct CredentialContext {
    ERL_NIF_TERM credentials_source;  // Can be MFA tuple or direct creds map/list
    ErlNifEnv*   callback_env;        // Env for invoking Erlang callbacks
  };

  /**
   * Validate that a credentials parameter has one of the recognized shapes:
   * - {Module, Function, Args} - MFA callback tuple
   * - #{type => T, ...} - Direct credentials map
   * - [{type, T}, ...] - Direct credentials proplist
   * - [] - explicitly "no credentials" (equivalent to omitting the option)
   *
   * This only validates the *shape*; it does not touch any credential
   * state - that's set up by set_{clone,fetch,push}_credentials_callback
   * via reset_credential_state(), which also covers the "no credentials
   * supplied at all" case. Returns false if the format is unrecognized.
   */
  bool parse_credentials_param(ErlNifEnv* env, ERL_NIF_TERM creds_term) {
    if (enif_is_empty_list(env, creds_term))
      return true;  // Explicitly no credentials

    int arity;
    const ERL_NIF_TERM* tuple;

    // {Module, Function, Args} - MFA callback tuple
    if (enif_get_tuple(env, creds_term, &arity, &tuple) && arity == 3)
      return true;

    // Map or proplist (direct credentials)
    return enif_is_map(env, creds_term) || enif_is_list(env, creds_term);
  }

  /**
   * Extract a binary/string value from credentials map/list
   */
  bool get_creds_string(ErlNifEnv* env, ERL_NIF_TERM creds,
                        ERL_NIF_TERM key_or_atom, std::string& out) {
    ERL_NIF_TERM val;
    ErlNifBinary bin;

    // Try as a map first
    if (enif_is_map(env, creds)) {
      if (enif_get_map_value(env, creds, key_or_atom, &val)) {
        if (enif_inspect_binary(env, val, &bin)) {
          out = bin_to_str(bin);
          return true;
        }
      }
    }
    // Try as a proplist
    else if (enif_is_list(env, creds)) {
      ERL_NIF_TERM head, tail = creds;
      while (enif_get_list_cell(env, tail, &head, &tail)) {
        const ERL_NIF_TERM* kv;
        int kv_arity;
        if (enif_get_tuple(env, head, &kv_arity, &kv) && kv_arity == 2) {
          if (enif_is_identical(kv[0], key_or_atom)) {
            if (enif_inspect_binary(env, kv[1], &bin)) {
              out = bin_to_str(bin);
              return true;
            }
          }
        }
      }
    }

    return false;
  }

  /**
   * Extract the credential type from credentials
   * Returns the atom (ssh_key, userpass, token, ssh_agent) or 0 if not found
   */
  ERL_NIF_TERM get_creds_type(ErlNifEnv* env, ERL_NIF_TERM creds) {
    ERL_NIF_TERM val;

    // Try as a map first
    if (enif_is_map(env, creds)) {
      if (enif_get_map_value(env, creds, ATOM_TYPE, &val)) {
        return val;
      }
    }
    // Try as a proplist
    else if (enif_is_list(env, creds)) {
      ERL_NIF_TERM head, tail = creds;
      while (enif_get_list_cell(env, tail, &head, &tail)) {
        const ERL_NIF_TERM* kv;
        int kv_arity;
        if (enif_get_tuple(env, head, &kv_arity, &kv) && kv_arity == 2) {
          if (enif_is_identical(kv[0], ATOM_TYPE)) {
            return kv[1];
          }
        }
      }
    }

    return 0;
  }

  /**
   * Create an SSH key credential from Erlang data
   * Supports both paths and in-memory key content
   */
  int create_ssh_key_credential(git_credential** cred, ErlNifEnv* env,
                                 ERL_NIF_TERM creds_term) {
    std::string username, privkey, pubkey, passphrase;

    // Extract required fields
    if (!get_creds_string(env, creds_term, ATOM_USERNAME, username)) {
      return GIT_EAUTH;  // Missing username
    }
    if (!get_creds_string(env, creds_term, ATOM_PRIVKEY, privkey)) {
      return GIT_EAUTH;  // Missing privkey
    }

    // Extract optional fields (default to NULL)
    get_creds_string(env, creds_term, ATOM_PUBKEY, pubkey);
    get_creds_string(env, creds_term, ATOM_PASSPHRASE, passphrase);

    // Try ssh_key_memory first (for in-memory PEM content)
    // If that fails, try ssh_key (for file paths)
    int ret = git_credential_ssh_key_memory_new(
      cred,
      username.c_str(),
      pubkey.empty() ? nullptr : pubkey.c_str(),
      privkey.c_str(),
      passphrase.empty() ? nullptr : passphrase.c_str());

    if (ret == GIT_OK) {
      return GIT_OK;
    }

    // Fallback to ssh_key for file paths
    return git_credential_ssh_key_new(
      cred,
      username.c_str(),
      pubkey.empty() ? nullptr : pubkey.c_str(),
      privkey.c_str(),
      passphrase.empty() ? nullptr : passphrase.c_str());
  }

  /**
   * Create a username/password credential from Erlang data
   */
  int create_userpass_credential(git_credential** cred, ErlNifEnv* env,
                                  ERL_NIF_TERM creds_term) {
    std::string username, password;

    if (!get_creds_string(env, creds_term, ATOM_USERNAME, username)) {
      return GIT_EAUTH;
    }
    if (!get_creds_string(env, creds_term, ATOM_PASSWORD, password)) {
      return GIT_EAUTH;
    }

    return git_credential_userpass_plaintext_new(cred, username.c_str(), password.c_str());
  }

  /**
   * Create an SSH agent credential from Erlang data
   */
  int create_ssh_agent_credential(git_credential** cred, ErlNifEnv* env,
                                   ERL_NIF_TERM creds_term) {
    std::string username;

    if (!get_creds_string(env, creds_term, ATOM_USERNAME, username)) {
      return GIT_EAUTH;
    }

    return git_credential_ssh_key_from_agent(cred, username.c_str());
  }

  /**
   * Create a credential from direct Erlang data (map or proplist)
   * Supports: ssh_key, userpass, token (as userpass), ssh_agent
   */
  int create_credential_from_direct(git_credential** cred, ErlNifEnv* env,
                                     ERL_NIF_TERM creds_term) {
    ERL_NIF_TERM cred_type = get_creds_type(env, creds_term);

    if (enif_is_identical(cred_type, ATOM_SSH_KEY)) {
      return create_ssh_key_credential(cred, env, creds_term);
    }
    else if (enif_is_identical(cred_type, ATOM_USERPASS)) {
      return create_userpass_credential(cred, env, creds_term);
    }
    else if (enif_is_identical(cred_type, ATOM_TOKEN)) {
      // Treat token as userpass (token is username, token value is password)
      std::string username, token;
      if (!get_creds_string(env, creds_term, ATOM_USERNAME, username)) {
        return GIT_EAUTH;
      }
      if (!get_creds_string(env, creds_term, ATOM_TOKEN, token)) {
        return GIT_EAUTH;
      }
      return git_credential_userpass_plaintext_new(cred, username.c_str(), token.c_str());
    }
    else if (enif_is_identical(cred_type, ATOM_SSH_AGENT)) {
      return create_ssh_agent_credential(cred, env, creds_term);
    }

    return GIT_EAUTH;  // Unknown credential type
  }

  // TODO: Implement Erlang callback invocation
  // This will require careful environment setup and thread-local storage
  // to safely call Erlang from within a libgit2 callback.
  // For now, we only support direct credentials (map/list format).
  // MFA callbacks can be implemented in a future phase.

  inline bool file_readable(std::string const& path) {
    struct stat st;
    return !path.empty() && ::stat(path.c_str(), &st) == 0;
  }

  /**
   * Try each of OpenSSH's conventional default identity files, in the same
   * order `ssh`/`git` itself would offer them, skipping any that don't
   * exist. Only reached once ssh-agent has already been tried by the
   * caller (create_default_credential) and this is attempt N>1, so that the
   * agent - the more dynamic, revocable option - is always preferred first.
   */
  int try_default_identity_files(git_credential** cred, const char* username) {
    const char* home = std::getenv("HOME");
    if (!home || !*home)
      return GIT_PASSTHROUGH;

    static const char* kNames[] = {"id_ed25519", "id_ecdsa", "id_rsa"};

    for (auto name : kNames) {
      std::string priv = std::string(home) + "/.ssh/" + name;
      std::string pub  = priv + ".pub";
      if (!file_readable(priv))
        continue;
      // A matching .pub is preferred but not required by libgit2/libssh2.
      int rc = git_credential_ssh_key_new(
        cred, username, file_readable(pub) ? pub.c_str() : nullptr, priv.c_str(), nullptr);
      if (rc == GIT_OK)
        return GIT_OK;
    }
    return GIT_PASSTHROUGH;
  }

  /**
   * Best-effort default credential, mirroring what the `git` CLI does when
   * no explicit credentials are configured: delegate to ssh-agent, then fall
   * back to the conventional default SSH identity files, then supply the
   * username libgit2 asked for when that's all that's needed, or finally the
   * "default" (NTLM/Negotiate) credential. This matters because a transport
   * can end up needing SSH auth even when the caller only ever dealt with an
   * https:// URL - e.g. local git config `url."git@host:".insteadOf =
   * https://host/` rewrites the URL before libgit2 ever sees it. libgit2
   * requires *some* callback to be registered any time authentication is
   * requested; with a NULL callback it fails immediately with
   * "authentication required but no callback set" instead of trying the
   * transport's normal defaults.
   *
   * `attempt` is the 1-based retry count for this operation (see
   * g_credential_attempts): ssh-agent is retried on attempt 1 the same way
   * `ssh` retries it, and default identity files are only tried starting
   * on attempt 2 so a working agent is never shadowed by a stale key file.
   */
  int create_default_credential(git_credential** cred,
                                 const char* username_from_url,
                                 unsigned int allowed_types,
                                 int attempt) {
    const char* user = username_from_url ? username_from_url : "git";

    if (allowed_types & GIT_CREDENTIAL_SSH_KEY) {
      if (attempt <= 1 && git_credential_ssh_key_from_agent(cred, user) == GIT_OK)
        return GIT_OK;
      if (try_default_identity_files(cred, user) == GIT_OK)
        return GIT_OK;
    }
    if (allowed_types & GIT_CREDENTIAL_USERNAME && !username_from_url) {
      if (git_credential_username_new(cred, "git") == GIT_OK)
        return GIT_OK;
    }
    if (allowed_types & GIT_CREDENTIAL_DEFAULT) {
      if (git_credential_default_new(cred) == GIT_OK)
        return GIT_OK;
    }
    // Nothing we can offer - let libgit2/the transport report the failure.
    return GIT_PASSTHROUGH;
  }

  /**
   * Main credential acquire callback for libgit2
   * Called when libgit2 needs credentials
   *
   * Currently only supports direct credentials (map/list format).
   * MFA callbacks will be supported in a future phase.
   */
  int credential_acquire_callback(git_credential** cred,
                                  const char* url,
                                  const char* username_from_url,
                                  unsigned int allowed_types,
                                  void* payload) {
    // Cap retries: libgit2 re-invokes this callback as long as we keep
    // handing back credentials that fail; without a limit an operation
    // against a host we can't actually authenticate to would loop forever.
    if (++g_credential_attempts > MAX_CREDENTIAL_ATTEMPTS)
      return GIT_PASSTHROUGH;

    bool have_source = g_credentials_source &&
      !enif_is_empty_list(g_callback_env, g_credentials_source);

    if (have_source) {
      // Check if it's direct credentials (map or list)
      if (enif_is_map(g_callback_env, g_credentials_source) ||
          enif_is_list(g_callback_env, g_credentials_source)) {
        // It's direct credentials - create credential from them
        return create_credential_from_direct(cred, g_callback_env, g_credentials_source);
      }
      // For now, MFA callbacks fall through to default auth
      // TODO: Implement safe Erlang callback invocation
    }

    // No (usable) credentials supplied - fall back to the same defaults
    // `git` itself would use (ssh-agent, default identity files,
    // username-only, NTLM/default).
    return create_default_credential(cred, username_from_url, allowed_types,
                                      g_credential_attempts);
  }

  /**
   * Reset the thread-local credential state for a new NIF call.
   *
   * These are thread_locals on the scheduler thread executing the NIF, and
   * scheduler threads are reused across many unrelated NIF calls, so state
   * left over from a previous clone/fetch/push must not leak into this one.
   * `credentials_source` may be the "no credentials supplied" sentinel (0),
   * in which case the credential callback falls back to defaults below.
   */
  inline void reset_credential_state(ErlNifEnv* env, ERL_NIF_TERM credentials_source) {
    g_callback_env        = env;
    g_credentials_source  = credentials_source;
    g_credential_attempts = 0;
  }

  /**
   * Set up credential callback for clone options.
   *
   * The callback is installed unconditionally - even with no credentials
   * supplied - so that libgit2 always has *something* to call. Without any
   * callback registered, libgit2 refuses authentication requests outright
   * (e.g. for an SSH transport reached via a `url.insteadOf` rewrite of an
   * https:// URL) instead of falling back to ssh-agent/default credentials
   * the way the `git` CLI does.
   */
  void set_clone_credentials_callback(ErlNifEnv* env, git_clone_options* opts,
                                       ERL_NIF_TERM credentials_source) {
    reset_credential_state(env, credentials_source);

    opts->fetch_opts.callbacks.credentials = credential_acquire_callback;
    opts->fetch_opts.callbacks.payload = nullptr;  // We use thread-local storage
  }

  /**
   * Set up credential callback for fetch options. See set_clone_credentials_callback.
   */
  void set_fetch_credentials_callback(ErlNifEnv* env, git_fetch_options* opts,
                                       ERL_NIF_TERM credentials_source) {
    reset_credential_state(env, credentials_source);

    opts->callbacks.credentials = credential_acquire_callback;
    opts->callbacks.payload = nullptr;
  }

  /**
   * Set up credential callback for push options. See set_clone_credentials_callback.
   */
  void set_push_credentials_callback(ErlNifEnv* env, git_push_options* opts,
                                      ERL_NIF_TERM credentials_source) {
    reset_credential_state(env, credentials_source);

    opts->callbacks.credentials = credential_acquire_callback;
    opts->callbacks.payload = nullptr;
  }

  /**
   * Extract credentials from options map
   * Returns the credentials term or 0 if not present
   */
  ERL_NIF_TERM extract_credentials_from_options(ErlNifEnv* env, ERL_NIF_TERM opts) {
    if (!enif_is_map(env, opts)) {
      return 0;  // Not a map
    }

    ERL_NIF_TERM creds;
    if (enif_get_map_value(env, opts, ATOM_CREDENTIALS, &creds)) {
      return creds;
    }

    return 0;  // No credentials in options
  }
}
