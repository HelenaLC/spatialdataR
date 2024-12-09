

library(rvest)
library(DT)
z = read_html("https://spatialdata.scverse.org/en/stable/tutorials/notebooks/datasets/README.html#id14")
thevec = z %>% html_elements("table") %>% html_elements("tr") %>% html_elements("p") %>% xml_text() 
mm = matrix(thevec, nr=7)
thed = data.frame(t(mm)[-c(1,10),]) 
names(thed) = t(mm)[1,,drop=TRUE]
datatable(thed[-c(2:4),-c(5,6)])


#PC002284:BASEL_2024 vincentcarey$ ls -tl ~/Library/Caches/org*R/R/BiocFileCache/*2fov*
#-rw-r--r--  1 vincentcarey  staff  379018013 Dec  4 16:49 /Users/vincentcarey/Library/Caches/org.R-project.R/R/BiocFileCache/a2c920930058_Xenium_V1_human_Breast_2fov_outs.zip
#-rw-r--r--  1 vincentcarey  staff  277997802 Nov 22 06:09 /Users/vincentcarey/Library/Caches/org.R-project.R/R/BiocFileCache/630c1fc66ec9_Xenium_V1_human_Lung_2fov_outs.zip
#

#Xenium_V1, Breast (2 FOV), 380 MB, 
#Xenium_V1_human_Breast_2fov
#CCA
newf = data.frame(Technology="Xenium (trimmed)", Sample="Breast (2 FOV)",
    `File Size`="380 MB", 
    `Filename (spatialdata-sandbox)`= "Xenium_V1_human_Breast_2fov",
     license="CCA", check.names=FALSE)
newf2 = data.frame(Technology="Xenium (trimmed)", Sample="Lung (2 FOV)",
    `File Size`="280 MB", 
    `Filename (spatialdata-sandbox)`= "Xenium_V1_human_Lung_2fov",
     license="CCA", check.names=FALSE)

newd = thed[-c(2:4), -c(5,6)]
newd = do.call(rbind, list(newd, newf, newf2))


# https://www.10xgenomics.com/support/software/xenium-onboard-analysis/latest/resources/xenium-example-data
