



```
devtools::install_github("adrienPAVOINE/ClustCheck")
library(ClustCheck)
```
First of all, you need to import a dataset (with numerics, categoricals variables or both of them). For exemple, let's try with the BankCustomer dataset. A Data Frame included in the ClustCheck package.
```
BankCustomer <- ClustCheck::BankCustomer
```
It's contain a set of a bank's clients with numeric AND categorical variables. 
To test the package with your data frame, you have to call the Dataset() function to create your class object. It's take in input a dataframe and the vector with your predict cluster groups. It's can also take in input the vector with the reels cluster groups (usefull for only one function : Validation()).
```
object <- ClustCheck::Dataset(BankCustomer, BankCustomer$Cluster)
```
