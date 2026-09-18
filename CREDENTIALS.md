# egit Authentication and Credentials Guide

This guide covers how to use egit to access private Git repositories using various authentication methods.

## Table of Contents

- [Quick Start](#quick-start)
- [Supported Authentication Methods](#supported-authentication-methods)
- [SSH Key Authentication](#ssh-key-authentication)
- [Username/Password Authentication](#usernamepassword-authentication)
- [Personal Access Tokens](#personal-access-tokens)
- [SSH Agent](#ssh-agent)
- [API Reference](#api-reference)
- [Best Practices](#best-practices)
- [Troubleshooting](#troubleshooting)

## Quick Start

Clone a private repository with SSH key authentication:

```erlang
1> Creds = #{
  type => ssh_key,
  username => <<"git">>,
  privkey => <<"/home/user/.ssh/id_rsa">>,
  pubkey => <<"/home/user/.ssh/id_rsa.pub">>
}.

2> Repo = git:clone(
  <<"git@github.com:user/private-repo.git">>,
  <<"/tmp/private-repo">>,
  #{credentials => Creds}
).

3> git:fetch(Repo, <<"origin">>, #{credentials => Creds}).

4> git:push(Repo, <<"origin">>, [<<"main">>], #{credentials => Creds}).
```

## Supported Authentication Methods

egit supports four credential types:

| Type | URL Scheme | Use Case |
|------|-----------|----------|
| `ssh_key` | `git@...` | SSH key-based authentication |
| `userpass` | `https://...` | HTTP basic authentication |
| `token` | `https://...` | Personal access tokens (GitHub, GitLab, etc.) |
| `ssh_agent` | `git@...` | Delegate to system SSH agent |

## SSH Key Authentication

### SSH Key from Files

Use SSH keys from file paths on the filesystem:

```erlang
Creds = #{
  type => ssh_key,
  username => <<"git">>,
  privkey => <<"/home/user/.ssh/id_rsa">>,
  pubkey => <<"/home/user/.ssh/id_rsa.pub">>,
  passphrase => <<"optional_passphrase">>
}.

Repo = git:clone(
  <<"git@github.com:user/private-repo.git">>,
  <<"/tmp/private-repo">>,
  #{credentials => Creds}
).
```

**Fields:**
- `type` (required): `ssh_key`
- `username` (required): Git username (typically `"git"` for GitHub/GitLab)
- `privkey` (required): Path to private key file or PEM content
- `pubkey` (optional): Path to public key file or SSH public key content
- `passphrase` (optional): Passphrase if key is encrypted

### SSH Key from Memory

Use SSH key material directly in memory (useful for Docker/Kubernetes):

```erlang
PrivateKeyPEM = <<"-----BEGIN RSA PRIVATE KEY-----
MIIEpAIBAAKCAQEA1234567890...
... (PEM content)
-----END RSA PRIVATE KEY-----">>,

PublicKeySSH = <<"ssh-rsa AAAAB3NzaC1yc2EAAAADAQAB...">>,

Creds = #{
  type => ssh_key,
  username => <<"git">>,
  privkey => PrivateKeyPEM,
  pubkey => PublicKeySSH
}.

Repo = git:clone(Url, Path, #{credentials => Creds}).
```

### Auto-Detection

egit automatically detects whether the key material is a file path or PEM content:

```erlang
%% libgit2 will first try ssh_key_memory_new (for PEM content)
%% If that fails, it will try ssh_key_new (for file paths)
```

### With Encrypted Keys

For passphrase-protected SSH keys:

```erlang
Creds = #{
  type => ssh_key,
  username => <<"git">>,
  privkey => <<"/home/user/.ssh/id_rsa">>,
  pubkey => <<"/home/user/.ssh/id_rsa.pub">>,
  passphrase => <<"my_passphrase">>  %% Provide passphrase here
}.

Repo = git:clone(Url, Path, #{credentials => Creds}).
```

## Username/Password Authentication

For HTTP(S) repositories with basic authentication:

```erlang
Creds = #{
  type => userpass,
  username => <<"github_user">>,
  password => <<"personal_access_token_or_password">>
}.

Repo = git:clone(
  <<"https://github.com/user/private-repo.git">>,
  <<"/tmp/private-repo">>,
  #{credentials => Creds}
).

git:fetch(Repo, <<"origin">>, #{credentials => Creds}).

git:push(Repo, <<"origin">>, [<<"main">>], #{credentials => Creds}).
```

**Fields:**
- `type` (required): `userpass`
- `username` (required): GitHub/GitLab username
- `password` (required): Password or personal access token

**Note:** For security, use personal access tokens instead of passwords.

## Personal Access Tokens

### GitHub Personal Access Token

```erlang
%% Create at: https://github.com/settings/tokens
%% Select scopes: repo (full control of private repositories)

TokenCreds = #{
  type => token,
  username => <<"oauth2">>,  %% or any username
  token => <<"ghp_1234567890abcdefghijklmnopqrstuvwxyz">>
}.

%% HTTPS URL required for token-based auth
Repo = git:clone(
  <<"https://github.com/user/private-repo.git">>,
  <<"/tmp/private-repo">>,
  #{credentials => TokenCreds}
).
```

### GitLab Personal Access Token

```erlang
%% Create at: https://gitlab.com/-/profile/personal_access_tokens
%% Select scopes: api, read_repository, write_repository

TokenCreds = #{
  type => token,
  username => <<"gitlab-ci-token">>,
  token => <<"glpat-1234567890abcdefghij">>
}.

Repo = git:clone(
  <<"https://gitlab.com/user/private-repo.git">>,
  <<"/tmp/private-repo">>,
  #{credentials => TokenCreds}
).
```

### Other Git Hosting Services

For any service supporting token-based HTTPS auth:

```erlang
TokenCreds = #{
  type => token,
  username => <<"username_or_oauth2">>,
  token => <<"your_token_here">>
}.
```

## SSH Agent

Delegate authentication to the system SSH agent:

```erlang
%% Requires ssh-agent running with your keys loaded:
%% $ ssh-agent
%% $ ssh-add ~/.ssh/id_rsa

AgentCreds = #{
  type => ssh_agent,
  username => <<"git">>
}.

Repo = git:clone(
  <<"git@github.com:user/private-repo.git">>,
  <<"/tmp/private-repo">>,
  #{credentials => AgentCreds}
).
```

**Note:** On macOS, SSH keys are automatically managed by the system and added to ssh-agent.

## API Reference

### Credentials Data Structure

#### Map Format (Recommended)

```erlang
%% SSH Key
#{
  type => ssh_key,
  username => <<"git">>,
  privkey => <<"path_or_content">>,
  pubkey => <<"path_or_content">>,  % Optional
  passphrase => <<"pass">>           % Optional
}

%% Username/Password
#{
  type => userpass,
  username => <<"user">>,
  password => <<"pass">>
}

%% Token
#{
  type => token,
  username => <<"user">>,
  token => <<"token_value">>
}

%% SSH Agent
#{
  type => ssh_agent,
  username => <<"git">>
}
```

#### Proplist Format

All formats can also be specified as proplists:

```erlang
[
  {type, ssh_key},
  {username, <<"git">>},
  {privkey, <<"path">>},
  {pubkey, <<"path">>}
]
```

### Functions

#### clone/3

Clone a repository with credentials:

```erlang
-spec clone(binary()|string(), binary()|string(), Options :: map()) ->
  repository() | {error, term()}.

git:clone(Url, Path, #{credentials => Creds}).
```

#### fetch/3

Fetch from a remote with credentials:

```erlang
-spec fetch(repository(), binary()|string(), Options :: map()) ->
  ok | {error, term()}.

git:fetch(Repo, <<"origin">>, #{credentials => Creds}).
```

#### pull/3

Pull from a remote with credentials:

```erlang
-spec pull(repository(), binary()|string(), Options :: map()) ->
  ok | {error, term()}.

git:pull(Repo, <<"origin">>, #{credentials => Creds}).
```

#### push/4

Push to a remote with credentials:

```erlang
-spec push(repository(), binary()|string(), [binary()|string()], Options :: map()) ->
  ok | {error, term()}.

git:push(Repo, <<"origin">>, [<<"refs/heads/main">>], #{credentials => Creds}).
```

## Best Practices

### 1. Use SSH Keys Over Passwords

SSH keys are more secure than passwords:

```erlang
%% Good: SSH key
Creds = #{type => ssh_key, username => <<"git">>, privkey => Path}.

%% Avoid: Plain password
% Creds = #{type => userpass, username => <<"user">>, password => <<"pass">>}.
```

### 2. Use Personal Access Tokens Over Passwords

For HTTPS, use PATs instead of passwords:

```erlang
%% Good: Personal access token
TokenCreds = #{type => token, username => <<"user">>, token => Token}.

%% Avoid: Password
% Creds = #{type => userpass, username => <<"user">>, password => Password}.
```

### 3. Protect Sensitive Data

Never hardcode credentials in your source:

```erlang
%% Good: Load from environment
PrivateKey = os:getenv("GITHUB_SSH_KEY"),
Creds = #{type => ssh_key, username => <<"git">>, privkey => PrivateKey}.

%% Good: Load from file
{ok, KeyContent} = file:read_file("/etc/secrets/github_key"),
Creds = #{type => ssh_key, username => <<"git">>, privkey => KeyContent}.

%% Avoid: Hardcoded credentials
% Creds = #{type => token, token => <<"ghp_1234567890...">>}.
```

### 4. Use SSH Agent in CI/CD

In CI/CD pipelines, use SSH agent:

```erlang
%% Docker/Kubernetes with SSH agent socket
AgentCreds = #{type => ssh_agent, username => <<"git">>},
Repo = git:clone(Url, Path, #{credentials => AgentCreds}).
```

### 5. Default Authentication

When no credentials are provided, egit falls back to system-level auth:

```erlang
%% Uses SSH agent, ~/.ssh/config, git credential helper
Repo = git:clone(Url, Path).
git:fetch(Repo, <<"origin">>).
git:push(Repo, <<"origin">>, [<<"main">>]).
```

### 6. Scope Tokens Appropriately

When creating PATs, use the minimum required scopes:

**GitHub:**
- Clone private repos: `repo` scope (full control)
- Push to private repos: `repo` scope
- Deploy key for read-only: Create deploy key instead

**GitLab:**
- Read-only: `read_repository`
- Write access: `write_repository`
- Full access: `api`

## Troubleshooting

### "authentication required but no callback set"

The library needs credentials for the repository. Provide credentials in the options map:

```erlang
%% Wrong: No credentials
Repo = git:clone(<<"git@github.com:user/private-repo.git">>, Path).

%% Correct: Provide credentials
Creds = #{type => ssh_key, username => <<"git">>, privkey => KeyPath},
Repo = git:clone(
  <<"git@github.com:user/private-repo.git">>,
  Path,
  #{credentials => Creds}
).
```

### SSH Key Not Found

Verify the key path is correct:

```erlang
%% Check if key file exists
file:read_file("/home/user/.ssh/id_rsa").

%% Use absolute path
Creds = #{
  type => ssh_key,
  username => <<"git">>,
  privkey => <<"/home/user/.ssh/id_rsa">>  %% Absolute path
}.
```

### "Permission denied" with SSH Keys

Verify the key has the correct permissions:

```bash
# SSH keys should be readable only by owner
chmod 600 ~/.ssh/id_rsa
chmod 644 ~/.ssh/id_rsa.pub
```

### Token Expired or Invalid

Check if your personal access token is still valid:

**GitHub:**
- Visit https://github.com/settings/tokens
- Check expiration date
- Regenerate if needed

**GitLab:**
- Visit https://gitlab.com/-/profile/personal_access_tokens
- Check expiration date
- Create new token if needed

### HTTP(S) with SSH URL

If using HTTPS credentials with an SSH URL, switch the URL format:

```erlang
%% Wrong: Token credentials with SSH URL
TokenCreds = #{type => token, username => <<"user">>, token => Token},
% Repo = git:clone(<<"git@github.com:user/repo.git">>, Path, #{credentials => TokenCreds}).

%% Correct: Use HTTPS URL
Repo = git:clone(
  <<"https://github.com/user/repo.git">>,
  Path,
  #{credentials => TokenCreds}
).
```

### SSH Keys with Passphrase

Ensure passphrase is provided for encrypted keys:

```erlang
%% Encrypted key without passphrase will fail
Creds = #{
  type => ssh_key,
  username => <<"git">>,
  privkey => <<"/home/user/.ssh/id_rsa">>,
  passphrase => <<"correct_passphrase">>  %% Must provide passphrase
}.
```

### SSH Agent Not Found

Ensure SSH agent is running:

```bash
# Start SSH agent
eval "$(ssh-agent -s)"

# Add your key
ssh-add ~/.ssh/id_rsa

# Verify key is loaded
ssh-add -l
```

Then use SSH agent credentials:

```erlang
AgentCreds = #{type => ssh_agent, username => <<"git">>},
Repo = git:clone(Url, Path, #{credentials => AgentCreds}).
```

## Examples

### Complete Workflow: Private Mono-Repository

```erlang
-module(repo_manager).

setup_and_sync(Url, LocalPath, PrivateKeyPath) ->
  %% Create credentials
  Creds = #{
    type => ssh_key,
    username => <<"git">>,
    privkey => PrivateKeyPath,
    pubkey => PrivateKeyPath ++ ".pub"
  },

  %% Clone repository
  Repo = git:clone(Url, LocalPath, #{credentials => Creds}),

  %% Create feature branch
  ok = git:branch_create(Repo, "feature/new-work"),
  ok = git:checkout(Repo, "feature/new-work"),

  %% Make changes
  ok = file:write_file(
    LocalPath ++ "/new_file.txt",
    "Important work\n"
  ),

  %% Stage and commit
  #{files := Files} = git:add(Repo, "."),
  io:format("Staged: ~w~n", [Files]),

  {ok, CommitOID} = git:commit(Repo, "Add new feature"),
  io:format("Committed: ~s~n", [CommitOID]),

  %% Push with credentials
  ok = git:push(
    Repo,
    <<"origin">>,
    [<<"refs/heads/feature/new-work:refs/heads/feature/new-work">>],
    #{credentials => Creds}
  ),

  io:format("Pushed to remote~n"),
  Repo.
```

### Docker Environment

```erlang
%% In Docker/Kubernetes with SSH agent socket
docker_clone(Url, Path) ->
  %% SSH agent socket is typically at /run/user/1000/ssh_agent
  AgentCreds = #{type => ssh_agent, username => <<"git">>},
  git:clone(Url, Path, #{credentials => AgentCreds}).
```

### GitHub Actions CI/CD

```erlang
%% In GitHub Actions with SSH key in secret
ci_clone(Url, Path) ->
  %% Secrets are available as environment variables
  PrivateKey = os:getenv("GITHUB_SSH_KEY"),
  Creds = #{
    type => ssh_key,
    username => <<"git">>,
    privkey => PrivateKey
  },
  git:clone(Url, Path, #{credentials => Creds}).
```

## See Also

- [egit README](README.md) - Main documentation
- [git module documentation](https://hexdocs.pm/egit/git.html) - API reference
- [libgit2 credentials](https://libgit2.org/docs/guides/authentication/) - Underlying library docs
