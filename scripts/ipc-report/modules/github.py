"""Simple Github API wrapper"""

import requests

from zipfile import ZipFile
from io import BytesIO


class ApiGroup:
    """Internal API group base class."""

    def __init__(self, api: "GitHub"):
        self.api = api


class Actions(ApiGroup):
    """Internal Actions API group."""

    def get_workflow_run(self, owner: str, repo: str, run_id: int) -> dict:
        """Get a workflow run."""
        return self.api.get(f"repos/{owner}/{repo}/actions/runs/{run_id}")

    def list_workflow_runs(
        self,
        owner: str,
        repo: str,
        *,
        actor: str | None = None,
        branch: str | None = None,
        event: str | None = None,
        status: str | None = None,
        per_page: int = 30,
        page: int = 1,
        created: str | None = None,
        exclude_pull_requests: bool | None = None,
        check_suite_id: int | None = None,
        head_sha: str | None = None,
    ) -> dict:
        """List workflow runs for a repository."""
        return self.api.get(
            f"repos/{owner}/{repo}/actions/runs",
            params={
                "actor": actor,
                "branch": branch,
                "event": event,
                "status": status,
                "per_page": per_page,
                "page": page,
                "created": created,
                "exclude_pull_requests": exclude_pull_requests,
                "check_suite_id": check_suite_id,
                "head_sha": head_sha,
            },
        )

    def list_workflow_run_artifacts(
        self,
        owner: str,
        repo: str,
        run_id: int,
        *,
        per_page: int = 30,
        page: int = 1,
        name: str | None = None,
        direction: str | None = None,
    ) -> dict:
        """List artifacts for a workflow run."""
        return self.api.get(
            f"repos/{owner}/{repo}/actions/runs/{run_id}/artifacts",
            params={
                "per_page": per_page,
                "page": page,
                "name": name,
                "direction": direction,
            },
        )

    def download_artifact(
        self,
        owner: str,
        repo: str,
        artifact_id: int,
        archive_format: str = "zip",
    ) -> ZipFile | bytes:
        """Download an artifact."""
        response = self.api.get_raw(
            f"repos/{owner}/{repo}/actions/artifacts/{artifact_id}/{archive_format}",
        )
        if response.headers.get("Content-Type") == "application/zip":
            return ZipFile(BytesIO(response.content))
        else:
            return response.content


class Commits(ApiGroup):
    """Internal Commits API group."""

    def list_commits(
        self,
        owner: str,
        repo: str,
        *,
        sha: str | None = None,
        path: str | None = None,
        author: str | None = None,
        committer: str | None = None,
        since: str | None = None,
        until: str | None = None,
        per_page: int = 30,
        page: int = 1,
    ) -> dict:
        """List commits for a repository."""
        return self.api.get(
            f"repos/{owner}/{repo}/commits",
            params={
                "sha": sha,
                "path": path,
                "author": author,
                "committer": committer,
                "since": since,
                "until": until,
                "per_page": per_page,
                "page": page,
            },
        )

    def get_commit(
        self,
        owner: str,
        repo: str,
        ref: str,
    ) -> dict:
        """Get a single commit."""
        return self.api.get(
            f"repos/{owner}/{repo}/commits/{ref}",
        )


class GitHub:
    """Simple GitHub API wrapper."""

    def __init__(
        self,
        token: str,
        *,
        url: str = "api.github.com",
        proto: str = "https",
        port: int = 443,
    ):
        self.__token = token
        self.__url = url
        self.__proto = proto
        self.__port = port

        self.actions = Actions(self)
        self.commits = Commits(self)

    def __request(self, method: str, endpoint: str, **kwargs) -> requests.Response:
        resp = requests.request(
            method,
            f"{self.__proto}://{self.__url}:{self.__port}/{endpoint}",
            timeout=30,
            headers={
                "Accept": "application/vnd.github+json",
                "Authorization": f"Bearer {self.__token}",
                "X-GitHub-Api-Version": "2022-11-28",
            },
            **kwargs,
        )
        resp.raise_for_status()
        return resp

    def get_raw(self, endpoint: str, **kwargs) -> requests.Response:
        """Get raw content from a GitHub API endpoint."""
        return self.__request("get", endpoint, **kwargs)

    def get(self, endpoint: str, **kwargs) -> dict:
        """Get JSON content from a GitHub API endpoint."""
        return self.__request("get", endpoint, **kwargs).json()

    def post(self, endpoint: str, **kwargs) -> dict:
        """Get JSON content from a GitHub API endpoint with POST."""
        return self.__request("post", endpoint, **kwargs).json()

    def patch(self, endpoint: str, **kwargs) -> dict:
        """Get JSON content from a GitHub API endpoint with PATCH."""
        return self.__request("patch", endpoint, **kwargs).json()
