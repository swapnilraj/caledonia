# Documentation map

This directory contains both current contracts and historical remediation
records. Start with the document that matches the question; the longer PRDs are
evidence, not prerequisites for understanding the system.

## Start here

| Reader or question | Read first | Then use |
| --- | --- | --- |
| Pull-request reviewer | [PR review guide](pr-review-guide.md) | [Data-flow audit](whole-diff-data-flow-audit.md) for complete call graphs |
| New contributor or coding agent | [Repository guide](../CLAUDE.md) | [Data-model architecture](data-model-architecture.md) and [agent instructions](../AGENTS.md) |
| Protocol or Emacs client author | [Protocol v1](protocol-v1.md) | [Machine output v1](machine-output-v1.md) for CLI consumers |
| CLI automation author | [Machine output v1](machine-output-v1.md) | [README](../README.md) for user-facing behavior |
| Operator investigating files or alarms | [Data recovery](data-recovery.md) | [Architecture](data-model-architecture.md) for ownership guarantees |
| Maintainer checking remediation completeness | [Requirement ledger](data-model-requirements.json) | [Simplicity PRD](data-model-simplification-prd.md) and [whole-diff PRD](whole-diff-simplicity-prd.md) |

## Which documents are authoritative?

For current behavior, use this order:

1. public `.mli` interfaces and versioned boundary implementations;
2. executable tests and architecture checks;
3. the current architecture and protocol/output contracts;
4. PRDs and audit narratives.

The [technical-debt PRD](technical-debt-remediation-prd.md) records the original
behavioral remediation. The [data-model PRD](data-model-simplification-prd.md)
records the ownership rewrite and its 56-, 63-, and 77-requirement checkpoints.
The older counts are dated completion history; the current machine-readable
ledger contains 77 complete requirements.

## Keeping documentation synchronized

A change must update the contract at the same boundary it changes:

- storage ownership or mutation semantics: architecture, recovery guide, and
  storage/repository tests;
- protocol grammar: `protocol-v1.md`, protocol encoders/decoders, server E2E,
  and Emacs tests;
- JSON, CSV, ICS, or S-expression output: `machine-output-v1.md` and CLI tests;
- intentional user-visible behavior: root README, changelog, and focused tests;
- a new architectural invariant: requirement ledger, source PRD, and
  architecture gate.

Do not copy a wire or output schema into a general architecture document. Link
to its versioned contract so there remains one detailed source of truth.
