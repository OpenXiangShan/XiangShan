# Frontend Functional Coverage Code

This directory contains implementation predicates registered by
`env/functional_coverage.py`. It does not define a second functional-coverage
methodology or coverage registry.

## Implementation map

- `env/functional_coverage.py`: loads the canonical registry, coordinates
  recorder sampling, and writes functional-coverage artifacts.
- `env/funcov/__init__.py`: shared registration and common predicate entry
  points.
- `env/funcov/py/`: Python predicate packages, organized by observation
  domain.
- `env/funcov/sv/`: SystemVerilog observation/bind sources used by the
  simulator-specific coverage flow; they do not create a second canonical
  registry.

Use `src/test/python/Frontend/docs/03_funcov_model/skills.md` for the
canonical testpoint, recorder, testcase, artifact, and back-annotation rules.
Use `src/test/python/Frontend/README.md` for the source-tree layout and script
entrypoints.
