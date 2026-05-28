# USDA Validation Study

## Summary
This project is aimed at validating RT-QuIC and Nano-QuIC for post- and ante-mortem testing of white-tail deer. This is a longitudinal study, with sampling efforts every 3 months.

### Ante-mortem samples:
-   Oral swabs
-   Nasal swabs
-   Rectoanalmucosa-associated lymph tissue (RAMALT)
-   Whole blood
-   Plasma
-   Serum
-   Ear punches
-   Feces

### Post-mortem samples:
-   Obex
-   RAMALT
-   RPLN

---

## Prerequisites
Before running anything, make sure the following are installed on your machine:

1. **R** (>= 4.2) — [download here](https://cran.r-project.org/).
2. An R IDE — either:
    - **Positron** — *Recommended* [download here](https://positron.posit.co/download.html), or -
    - **RStudio** — [download here](https://posit.co/download/rstudio-desktop/)
3. **Python** (>= 3.10) — [download here](https://www.python.org/downloads/). 
    - On Windows, check "Add Python to PATH" during install.
4. **Git** — [download here](https://git-scm.com/downloads).
5. An **Airtable API key** with read/write access to the project base (`app7KsgYl2jhOnYg7`). Ask the MNPRO Airtable maintainer if you don't have one.

### Clone the repository
Open a terminal (PowerShell on Windows, Terminal on macOS/Linux) and run:
```bash
git clone https://github.com/gage1145/usda-validation.git
cd usda-validation
```

---

## Environment Setup
This project uses **two** virtual environments — one for R (`renv`) and one for Python (`venv`). Set both up once, up front, before running any pipeline steps.

### R environment (`renv`)
This project uses `renv` to lock R package versions. Always work inside the `renv` environment so everyone gets the same results. It depends on the development version 3.0.4 of [quicR](https://github.com/gage1145/quicR/releases/tag/v3.0.4); `renv` should pull the right version automatically.

1. Open the project:
   - **RStudio:** open `usda-validation.Rproj`.
   - **Positron:** open the `usda-validation` folder.

   In both cases `renv` should activate automatically when an R session starts in the project.

2. In the R console, restore the locked packages:
   ```R
   # If renv didn't auto-activate, activate it manually.
   renv::activate()

   # Restore all locked packages. Run this the first time you set up the project.
   renv::restore()

   # quicR and airtabler sometimes fail to install through restore.
   # If you see errors about them, install manually:
   renv::install("gage1145/quicR")
   renv::install("bergant/airtabler")
   ```

### Python environment (`venv`)
Set up a Python virtual environment and install dependencies.

**Windows (PowerShell):**
```powershell
python -m venv .venv
.venv\Scripts\activate
pip install -r requirements.txt
```

**macOS / Linux:**
```bash
python -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
```

You should see `(.venv)` at the start of your prompt once it's activated. You'll need to re-activate it every time you open a new terminal.

---

## Git Workflow
**Never commit directly to `main`.** All work should happen on your own branch and be merged into `main` through a pull request (PR) on GitHub. This keeps `main` stable and gives someone else a chance to review changes before they land.

### 1. Make sure your local `main` is up to date
Before starting new work, pull the latest `main` so your branch starts from the most up-to-date code:
```bash
git checkout main
git pull origin main
```

### 2. Create a new branch for your work
Use a short, descriptive name. A common convention is `yourname/short-description` or `feature/short-description`:
```bash
git checkout -b gage/update-readme
```
This both creates the branch and switches you to it. Confirm with:
```bash
git branch
```
The branch with the `*` next to it is the one you're on.

### 3. Commit your work
Stage and commit changes in small, logical chunks. Write a clear message describing **why** the change was made, not just what:
```bash
git add path/to/file1 path/to/file2
git commit -m "Add troubleshooting table to README"
```
> Avoid `git add .` or `git add -A` — they can sweep in files you didn't mean to commit (like `.env`, large data files, or local notes).

### 4. Push your branch to GitHub
The first time you push a new branch, use `-u` to link it to the remote:
```bash
git push -u origin gage/update-readme
```
After that, `git push` is enough.

### 5. Open a pull request on GitHub
1. Go to the [repository on GitHub](https://github.com/gage1145/usda-validation).
2. GitHub usually shows a yellow banner offering to "Compare & pull request" for your recently pushed branch — click it. Otherwise, go to the **Pull requests** tab and click **New pull request**.
3. Set the **base** branch to `main` and the **compare** branch to your branch.
4. Give the PR a clear title and a short description: what changed, why, and anything a reviewer should look at carefully.
5. Click **Create pull request**.
6. Request a review from a teammate. Address any feedback by pushing more commits to the same branch — the PR updates automatically.
7. Once the PR is approved, click **Merge pull request** on GitHub.

### 6. Clean up after merging
After your PR is merged, delete the branch and pull the updated `main`:
```bash
git checkout main
git pull origin main
git branch -d gage/update-readme           # delete local branch
git push origin --delete gage/update-readme # delete remote branch (optional)
```

### What to do if you accidentally committed to `main`
Don't push. Move the commit to a new branch and reset `main`:
```bash
git branch gage/my-fix          # save your work on a new branch
git reset --hard origin/main    # reset local main to match GitHub
git checkout gage/my-fix        # continue work on the new branch
```
If you're not sure what state you're in, **stop and ask** before running `reset --hard` — it discards uncommitted changes.

---

## Airtable Integration
This project uses two clients to talk to Airtable:
- **Python:** [pyAirtable](https://pyairtable.readthedocs.io/en/stable/)
- **R:** [airtabler](https://github.com/bergant/airtabler)

You need to store your Airtable API key in **two** places — one for each language.

### 1. Create `.env` (for Python)
In the project root, create a file named `.env`:
```bash
# Windows (PowerShell)
New-Item .env

# macOS / Linux
touch .env
```
Open `.env` in a text editor and add:
```
KEY=your_airtable_api_key_here
```

### 2. Create `.Renviron` (for R)
In the project root, create a file named `.Renviron`:
```bash
# Windows (PowerShell)
New-Item .Renviron

# macOS / Linux
touch .Renviron
```
Open `.Renviron` in a text editor and add:
```
AIRTABLE_API_KEY=your_airtable_api_key_here
```
> **Important:** The variable name **must** be `AIRTABLE_API_KEY` — `airtabler` will not find it otherwise. Restart R after creating the file.

You are now ready to work in the pipeline.

---

## Data Workflow
The pipeline has 6 steps:
1. Add raw Excel files into the appropriate sub-directory of `raw/`.
2. Extract the raw data into tidy-data formats (R).
3. Calculate kinetic metrics from the raw data (R).
4. Save results as compressed parquet files in `data/` (R).
5. Push results to Airtable (Python).
6. Analyze results by pulling from Airtable, which also contains metadata (R).

> **Important** — Step 5 can only be done by a user with write access to the Airtable base.

> Steps 2–4 are all handled by `curate.R`. Step 5 is handled by `airtable/update_results.py`. 

> Step 6 uses scripts in `scripts/`.

---

### Step 1 — Adding Raw Files
MARS-exported Excel files should be placed inside `raw/`. Choose the sub-folder based on the sample type:

| Sample type | Sub-folder |
|---|---|
| Whole blood R&D | `raw/blood/` |
| Serum / plasma R&D | `raw/serum-plasma/` |
| Anything with a MNPRO process ID | `raw/processedSamples/` |

If you're unsure where a file belongs, check what's already in each folder for a similar sample type.

---

### Step 2–4 — Extracting Raw Data and Saving Parquet Files
With the R environment set up (see [Environment Setup](#environment-setup)), run the curation script from the R console:
```R
# This does steps 2, 3, and 4.
source("curate.R")
```
When `curate.R` finishes, you'll see new `.parquet` files in `data/`. These are **not** committed to the repo (they can be regenerated anytime).

---

### Step 5 — Updating Airtable
With the Python environment activated (see [Environment Setup](#environment-setup)), push the parquet results to Airtable:
```bash
python airtable/update_results.py
```

> If you see an authentication error, double-check that `.env` exists in the project root and contains `KEY=...`.

---

### Step 6 — Analyzing Data from Airtable
All analysis is done in R. Make sure the `renv` environment is active and your `.Renviron` is set up (see [Airtable Integration](#airtable-integration)).

The `scripts/` folder contains one analysis/figures script per sample type, e.g.:
- `oral-swab_figures.R`
- `nasal-swab_figures.R`
- `ramalt_figures.R`, `ramalt_analysis.R`, `ramalt_roc.R`
- `blood_figures.R`, `blood_analysis.R`, `blood_roc.R`
- `serum-plasma_figures.R`, `serum-plasma_analysis.R`
- `necropsy_figures.R`

To run one:
```R
source("scripts/oral-swab_figures.R")
```

Figures are written to `figures/`.

#### Example: pulling data from Airtable yourself
```R
library(airtabler)

# Base ID for this project.
APP <- "app7KsgYl2jhOnYg7"

# Helper for building Airtable filterByFormula strings.
get_formula <- function(id, values, operator = "OR") {
  make_string <- function(value) sprintf("%s({%s} = %s", operator, id, value)
  sapply(values, make_string) |>
    paste(collapse = ",") |>
    paste0(strrep(")", length(values)), collapse = "")
}

# Pull the animals and results tables, filtering results to oral-swab samples.
tables  <- airtable(APP, c("animals", "results"))
animals <- tables$animals$select_all()
results <- tables$results$select_all(
  filterByFormula = get_formula(
    "sample_type", c("'MNPRO oral swab'", "'NADC oral swab'")
  )
)
```

---

## Troubleshooting

| Problem | Fix |
|---|---|
| `renv::restore()` fails on quicR or airtabler | Run `renv::install("gage1145/quicR")` and `renv::install("bergant/airtabler")` manually. |
| R can't find the Airtable key | Make sure `.Renviron` lives in the project root and the variable is named **exactly** `AIRTABLE_API_KEY`. Try running in the R console `usethis::edit_r_environ()`. Restart R after editing.|
| Python can't find the Airtable key | Make sure `.env` lives in the project root and contains `KEY=...`. |
| `python` command not found (Windows) | Reinstall Python and tick "Add Python to PATH". |
| `.venv\Scripts\activate` blocked on Windows | Run `Set-ExecutionPolicy -Scope CurrentUser RemoteSigned` in PowerShell, then retry. |
| `curate.R` runs but produces no parquet files | Check that your raw Excel files are in the right `raw/` sub-folder. |
