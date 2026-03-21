---
name: sendnix-skill
description: Guide for working on sendnix.de Docker environment via SSH. Use when working remotely on sendnix.de, managing Docker containers, or deploying services there.
---

# Sendnix Docker Environment

## Connection

All commands run via SSH:
```bash
ssh sendnix.de "command"
```

File transfers:
```bash
scp local_file sendnix.de:/remote/path
```

## Structure

```
/opt/docker/apps/
├── [service]/
│   ├── docker-compose.yml
│   ├── conf/           # Configuration (if any)
│   └── logs/           # Logs (if any)
└── ...
```

## Reverse Proxy

nginx-proxy with automatic HTTPS via letsencrypt.

Expose a service:
```yaml
environment:
  - VIRTUAL_HOST=myservice.sendnix.de
  - LETSENCRYPT_HOST=myservice.sendnix.de
```

## Common Operations

```bash
# List containers
ssh sendnix.de "docker ps"

# Logs
ssh sendnix.de "docker logs --tail 100 CONTAINER"

# Restart
ssh sendnix.de "cd /opt/docker/apps/SERVICE && docker compose restart"

# Rebuild
ssh sendnix.de "cd /opt/docker/apps/SERVICE && docker compose up -d --build"
```

## Mail Network

Connect container to send email:
```yaml
services:
  myservice:
    networks:
      - default
      - mail_default

networks:
  mail_default:
    external: true
```

Use `mail-mta-1:25` as SMTP host (no auth from Docker networks).
