BIN1000 retains its identity, definition, denominator, markers and historical results.

In c242/DefaultConfig, the legal CSR distribution fixes trigger timing to false;
the compiled FrontendTrigger ABI has no variable timing control. The complete
legacy timing-mismatch scenario is UNSUPPORTED_CURRENT_CONFIGURATION. This is
an applicability classification, not PASS, HIT, an alias fix or a denominator
subtraction. Do not synthesize the missing timing value in the sampler.

Frozen ccda4f1902499a07af957dcf155f24f7cafce697 preserves that distribution and
FrontendTrigger/TriggerCheckCanFire source semantics. Its new compiled ABI and
runtime qualification must be established independently. Old c242 artifacts
retain their original source/implementation identity.

Chain remains supported. Verify both-hit/end-fire and first-miss-or-disabled/
end-block cases in an independent API test using native five-field config,
same-lane hit/canFire/action and positive/negative controls. A chain-only PASS
is not a complete BIN1000 HIT. Keep the original legacy scenario and failures.
