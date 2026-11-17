import hashlib

import pytest

from .generate_random_asm import CaseSpec, FAMILIES, generate_case


@pytest.mark.parametrize("family", FAMILIES)
def test_seeded_generation_is_reproducible_and_hashes_the_source(family):
    spec = CaseSpec(family=family, seed=123, blocks=16, iterations=8)
    source, metadata = generate_case(spec)
    assert (source, metadata) == generate_case(spec)
    assert metadata["source_sha256"] == hashlib.sha256(source.encode("ascii")).hexdigest()
    assert metadata["spec"]["family"] == family
    assert "_start:" in source
