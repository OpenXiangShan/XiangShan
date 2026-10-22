from __future__ import annotations

from dataclasses import dataclass


@dataclass(frozen=True, slots=True)
class FailureContext:
    component: str
    phase: str
    cycle: int | None = None
    transaction_id: str | None = None

    def render(self) -> str:
        fields = [f"component={self.component}", f"phase={self.phase}"]
        if self.cycle is not None:
            fields.append(f"cycle={self.cycle}")
        if self.transaction_id is not None:
            fields.append(f"transaction={self.transaction_id}")
        return " ".join(fields)


class VerificationError(AssertionError):
    def __init__(self, context: FailureContext, message: str) -> None:
        super().__init__(f"{context.render()} {message}")
        self.context = context
        self.message = message


class IssueCollector:
    """Collect independent failures instead of retaining only the first error."""

    def __init__(self) -> None:
        self._issues: list[str] = []

    def add(self, context: FailureContext, message: str) -> None:
        self._issues.append(VerificationError(context, message).args[0])

    @property
    def issues(self) -> tuple[str, ...]:
        return tuple(self._issues)

    def raise_if_any(self, limit: int = 16) -> None:
        if not self._issues:
            return
        shown = self._issues[:limit]
        suffix = "" if len(shown) == len(self._issues) else f"; {len(self._issues) - limit} more"
        raise AssertionError("verification failures:\n- " + "\n- ".join(shown) + suffix)
