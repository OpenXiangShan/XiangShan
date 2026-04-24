# Frontend Interactive Web Console

## Start

```bash
TB_ENV_LOG_LEVEL=INFO ../scripts/run_web_console.sh
```

Open: `http://127.0.0.1:8000`

## Runtime knobs

- `TB_WEB_HOST` (default `127.0.0.1`)
- `TB_WEB_PORT` (default `8000`)
- `TB_ENV_LOG_LEVEL` (default `INFO`)

## Key capabilities

- Session start/reset
- FST waveform set/enable/flush/pause
- Clock step/run/pause
- Redirect/exception injection
- ICache/Uncache/PTW runtime config
- Real-time event stream via WebSocket (`/ws/events`)
- Snapshot polling API (`/api/state/snapshot`)
