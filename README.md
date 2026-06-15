# geo-orientation

This repository contains the R code and materials used to reproduce the tables and figures for the article
"Discrimination of Geological Orientation Data with Measurement Errors" by Di Marzio, Fensore, Panzera, Passamonti (2026).

# Files available
The folder 'R code' contains all relevant code.

The file "Fig1.r" provides the code to reproduce the shape of the following kernel functions: the von Mises density function and the twicing kernel, along with their deconvolution version obtained using both the Laplace and von Mises error.

The file "Fig2.r" provides the code to plot the measurement error models with zero mean and different concentration levels: Laplace
distribution (black) and von Mises density (red).

The file "Fig3.r" provides the code to represent the Equal-area projection of two groups of $L_0^1$ axes and the same data represented on the circumference of the circle based on the azimuthal component only.

The file "Fig4.r" provides the code to plot the density estimates of the azimuthal component for the two groups of $L_0^1$ axes orientations.

The file "Fig5.r" provides the code to represent the Equal-area projection of of axial-plane orientations and the same data represented on the circumference of the circle based on the dip component only.

The file "Fig6.r" provides the code to plot the density estimates of the dip angles for the two groups of axial-plane orientations.

The file "Tab1.r" provides the code to compute the misclassification rates (%) obtained using standard (KDE) and deconvoluted estimators
(vM, TW, JF), for the two groups of observations of $L_0^1$ axes.

The file "Tab2.r" provides the code to compute the misclassification rates (%) obtained using standard (KDE) and deconvoluted estimators
(vM, TW, JF), for the two groups of observations of axial-plane orientations.

