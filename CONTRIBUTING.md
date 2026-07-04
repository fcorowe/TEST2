# Contributing guidelines

Thank you for your interest in contributing to this project! We welcome all kinds of contributions—code, documentation, ideas and more. This guide will help you get started.


## How to contribute

All changes to `main` must come through pull requests. Direct pushes to `main`
are not part of the project workflow, including for maintainers and automation
agents. Francisco Rowe (`fcorowe`) and Carmen Cabrera (`carmen-cabrera`) can
review and merge accepted pull requests.

Every pull request should choose one merge route:

- **Review route:** this is the default route. The PR waits for an approving
  review from an eligible reviewer before merge.
- **Francisco maintainer-bypass route:** Francisco Rowe (`fcorowe`), as the
  main developer of `debiasR`, may explicitly request that his own PR, or a PR
  opened on his behalf, bypass the review wait and be merged or squash-merged
  directly when deadlines or reviewer availability require it. This exception
  applies only to Francisco. It still requires a branch, a pull request, a
  clear bypass statement in the PR body or discussion, and passing required
  checks unless Francisco explicitly accepts the remaining risk.

1. **Clone the repository**  
   ```bash
   git clone https://github.com/de-bias/debiasR.git
   cd debiasR
   ```

2. **Create a branch for your change**  
   Use a descriptive branch name. A `codex/` prefix is a good default for local work:
   ```bash
   git checkout -b codex/my-feature
   ```

3. **Make your changes**  
   Update the code, documentation, or tests as needed. When working on R code locally, it helps to load the package in place:
   ```r
   devtools::load_all(".")
   ```

4. **Run the relevant tests**  
   For the fast deterministic tier, use the curated runner documented in [notes/project-management/TEST_HEALTH.md](notes/project-management/TEST_HEALTH.md):
   ```bash
   Rscript scripts/run_fast_tests.R
   ```

5. **Commit your changes**  
   ```bash
   git add .
   git commit -m "Describe your changes"
   ```

6. **Push the branch**  
   ```bash
   git push origin codex/my-feature
   ```

7. **Open a Pull Request (PR)**  
   Use the PR template in `.github/pull_request_template.md` and link any related issues.
   Select either the review route or Francisco maintainer-bypass route in the
   template. Use the review route unless Francisco explicitly requests the
   bypass route.

---

## Using issue & PR templates

- When reporting a bug or requesting a feature, please use the issue template in `.github/ISSUE_TEMPLATE/`.
- When submitting a pull request, fill out the PR template to describe your changes and link related issues.

## Naming guide

- Use the current exported API in examples and docs: `adjust_*`, `validate_flow_overall()`, `validate_flow_pairs()` and `simulated_*`.
- If you need to mention older names for migration context, keep them in [notes/project-management/MIGRATION_MAP.md](notes/project-management/MIGRATION_MAP.md) rather than in user-facing instructions.
- Prefer the current vignette/file naming too, for example `adjust-inverse-penetration` rather than legacy `debias-method1`.


## Acknowledging contributors with All Contributors Bot

We celebrate all contributions! 

We use the [All Contributors Bot](https://allcontributors.org/) to recognise everyone’s work—code, docs, ideas and design.

### How to get acknowledged

- After your PR is merged, comment on an issue or PR:
  ```
  @all-contributors please add @your-username for code, doc
  ```
  (Replace `@your-username` and the contribution types as appropriate.)

- The bot will open a PR to update the contributors table in the README.  
- Review and merge that PR to see your avatar and contributions appear!

### Contribution types

See the [emoji key](https://allcontributors.org/docs/en/emoji-key) for all the ways you can be recognised.



---
  
We appreciate your contributions and support in building a vibrant community around this project. If you have any questions, feel free to reach out via issues or discussions.
