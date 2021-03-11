#!/usr/bin/env bash
set -euo pipefail

cmus-remote -C "add /home/alex/music"
cmus-remote -C "update-cache -f"
