# **SCADS May 2025: Machine Learning Training with R**  
*Regression & Classification using `tidyverse` and `tidymodels`*  

---

## **📑 Table of Contents**
- [Quick Start](#quick-start)
- [Prerequisites](#prerequisites)
- [Setup Guide](#setup-guide)
  - [1. Install Git](#install-git)
  - [2. Install R & RStudio](#install-r-rstudio)
  - [3. Clone the Repository](#clone-the-repository)
  - [4. Auto-Install Packages](#auto-install-packages)
- [Training Materials](#training-materials)
- [Dependency Management](#dependency-management)
- [Troubleshooting](#troubleshooting)
- [License & Attribution](#license-attribution)
- [Contact](#contact)

---

## **🚀 Quick Start** <a name="quick-start"></a>
1. **Install Git, R, and RStudio** ([instructions below](#prerequisites)).  
2. **Clone this repo** (RStudio: *File > New Project > Git* or `git clone`).  
3. **Run `renv::restore()`** in R to auto-install packages.  

---

### **✨ Why This Works**  
- **No Manual Setup**: `renv` handles dependencies.  
- **Beginner-Friendly**: Step-by-step guides for all OSes.  
- **Reproducible**: Quarto + `renv.lock` ensure consistency.  

---

## **📋 Prerequisites** <a name="prerequisites"></a>
| Tool          | Version       | Download Link                          |  
|:-------------:|:-------------:|:--------------------------------------:|  
| **Git**       | Latest        | <a href="https://git-scm.com" target="_blank">git-scm.com</a>     |  
| **R**         | ≥ 4.4.0       | <a href="https://cran.r-project.org" target="_blank">CRAN</a>     |  
| **RStudio**   | ≥ 2024.12.1   | <a href="https://www.rstudio.com/products/rstudio/download/" target="_blank">RStudio</a>     |  

---

## **⚙️ Setup Guide** <a name="setup-guide"></a>

### **1. Install Git** <a name="install-git"></a>
#### **Windows**  
- Download from <a href="https://git-scm.com/download/win" target="_blank">git-scm.com</a>.  
- Verify installation:  
  ```bash
  git --version
  ```  

#### **macOS/Linux**  
```bash
# macOS (Homebrew)
brew install git

# Linux (Debian/Ubuntu)
sudo apt update && sudo apt install git
```

---

### **2. Install R & RStudio** <a name="install-r-rstudio"></a>
- **R**: Download from <a href="https://cran.r-project.org" target="_blank">CRAN</a>.  
- **RStudio**: Download the free desktop version <a href="https://www.rstudio.com/products/rstudio/download/" target="_blank">here</a>.  

---

**📚 Learning Resources (Optional)** <a name="learning-resources-optional"></a> 

- **Detailed R/RStudio Installation Guide**:  
  Follow step-by-step instructions at <a href="https://bookdown.org/aepstk/intror/intro.html" target="_blank">bookdown.org/aepstk/intror/intro.html</a>.  
- **Learn Basic R Programming**:  
  Explore tutorials at <a href="https://bookdown.org/aepstk/intror/basicr.html" target="_blank">bookdown.org/aepstk/intror/basicr.html</a>.  

---

### **3. Clone the Repository** <a name="3-clone-the-repository"></a>
#### **Option A: RStudio (Recommended)**  
1. *File > New Project > Version Control > Git*.  
2. Paste URL:  
   ```
   https://github.com/aephidayatuloh/scads-may2025.git
   ```  

#### **Option B: Terminal**  
```bash
git clone https://github.com/aephidayatuloh/scads-may2025.git
cd scads-may2025
```

---

### **4. Auto-Install Packages** <a name="auto-install-packages"></a>
Run in R console:  
```R
renv::restore()  # Installs tidyverse, tidymodels, etc.
```  
✅ **Done!** Your environment is ready.  

---

## **📚 Training Materials** <a name="training-materials"></a>
| File                          | Description                     |  
|-------------------------------|---------------------------------|  
| [`01_ames_eda.qmd`](01_ames_eda.qmd) | Ames Housing EDA          |  
| [`02_ames_regression.qmd`](02_ames_regression.qmd) | Price Prediction   |  
| [`03_bank_eda.qmd`](03_bank_eda.qmd) | Bank Marketing EDA       |  
| [`04_bank_classification.qmd`](04_bank_classification.qmd) | Term Deposit Prediction |  

**How to Run**: Open any `.qmd` file → Click **Render** in RStudio.  

---

## **🔧 Dependency Management** <a name="dependency-management"></a>
The **`renv.lock`** ensures identical package versions across setups.  


---

## **❓ Troubleshooting** <a name="troubleshooting"></a>
### **Git Issues**  
- **RStudio doesn't detect Git?**  
  - Restart (close & open) RStudio.  
  - Windows: Verify Git path in *Tools > Global Options > Git/SVN*.  

### **Package Installation**  
- If `renv::restore()` fails:  
  ```R
  renv::init()  # Reinitialize environment
  ```  
- Check firewall/proxy settings if downloads fail.  

If you encounter any issues, please do not hesitate to contact your training instructor or assistants. We are here to help!

---

## **📜 License & Attribution** <a name="license-attribution"></a>
- **Code**: MIT License.  
- **Datasets**:  
  - **Ames Housing**: From R package [`modeldata`](https://tidymodels.github.io/modeldata/) (CC0 Public Domain)  
  - **Bank Marketing**: <a href="https://archive.ics.uci.edu/ml/datasets/Bank+Marketing" target="_blank">UCI Machine Learning Repository</a> (Open Data)  
 

---

## **📬 Contact** <a name="contact"></a>
- **Instructor**: Aep Hidayatuloh  
- **Email**: aephidayatuloh.mail@gmail.com  
- **Issues**: <a href="https://github.com/aephidayatuloh/scads-may2025/issues" target="_blank">GitHub Issues</a>  

---

