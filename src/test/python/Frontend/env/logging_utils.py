from __future__ import annotations

import logging
import os
from typing import Optional

ENV_LOGGER_NAME = "env"
ENV_LOG_LEVEL_VAR = "TB_ENV_LOG_LEVEL"


def parse_log_level(level: Optional[str]) -> int:
    if level is None:
        level = os.getenv(ENV_LOG_LEVEL_VAR, "INFO")
    text = str(level).strip().upper()
    return getattr(logging, text, logging.INFO)


def configure_env_logging(level: Optional[str] = None) -> int:
    resolved = parse_log_level(level)
    logger = logging.getLogger(ENV_LOGGER_NAME)
    logger.setLevel(resolved)
    return resolved


def get_env_logger(name: str) -> logging.Logger:
    return logging.getLogger(f"{ENV_LOGGER_NAME}.{name}")

