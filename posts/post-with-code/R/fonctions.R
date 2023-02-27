# 3).

#' Renvoie trois triangles issus de la première itération de Sierpiński
#' 
#' divide_triangle
#' 
#' @param A Longueur 1
#' @param B Longueur 2
#' @param C Longueur 3
#'
#' @return
#' @export


# Fonction pour calculer le milieu entre deux points
midpoint <- function(p1, p2) {
  return((p1 + p2) / 2)
}

# Fonction pour diviser un triangle en 3 triangles Sierpiński
divide_triangle <- function(A, B, C) {
  # Calculer les milieux de chaque côté
  AB <- midpoint(A, B)
  AC <- midpoint(A, C)
  BC <- midpoint(B, C)
  
  # Construire les trois triangles résultants
  triangle1 <- list(A, AC, AB)
  triangle2 <- list(B, AB, BC)
  triangle3 <- list(C, BC, AC)
  
  # Retourner les trois triangles
  return(list(triangle1, triangle2, triangle3))
}

# Exemple
A <- c(0, 0)
B <- c(2, 0)
C <- c(1, 2)
triangles <- divide_triangle(A, B, C)
print(triangles)




# 4).
#' Diviser une liste de triangles en trois triangles Sierpiński
#' 
#' divide_list_triangle
#' 
#' @param triangle_list une liste de triangles, où chaque triangle est une liste de trois points
#'
#' @return une nouvelle liste de triangles, résultant de la division de chacun des triangles de la liste initiale en trois triangles Sierpiński
#' @export
#' @example 
A <- c(0, 0)
B <- c(2, 0)
C <- c(1, 2)
triangles <- list(list(A, B, C))
new_triangles <- divide_list_triangle(triangles)
print(new_triangles)

divide_list_triangle <- function(triangle_list) {
  # Initialiser la liste de triangles résultants
  new_triangle_list <- list()
  
  # Pour chaque triangle de la liste initiale
  for (triangle in triangle_list) {
    # Appliquer la fonction divide_triangle
    divided_triangles <- divide_triangle(triangle[[1]], triangle[[2]], triangle[[3]])
    
    # Ajouter les triangles résultants à la liste
    new_triangle_list <- c(new_triangle_list, divided_triangles)
  }
  
  # Retourner la liste de triangles résultants
  return(new_triangle_list)
}







# 5).
remotes::install_github("3rakk/heron")
renv::snapshot()

# 6).
library(targets)
tar_script("posts/post-with-code/_targets.R")

# 7).
tar_config_set(store = "posts/post-with-code/_targets",
               script = "posts/post-with-code/_targets.R")

# 8).
list(
  
  # Target pour calculer les coordonnées des triangles après 4 itérations
  tar_target(
    triangles,
    {
      # Coordonnées des trois points initiaux
      A <- c(0, 0)
      B <- c(0, 1)
      C <- c(0.5, sqrt(3)/2)
      
      # Création de la liste initiale de triangles
      triangles <- list(list(A, B, C))
      
      # Itération de la fonction divide_list_triangle pour obtenir les triangles après 4 itérations
      for (i in 1:4) {
        triangles <- divide_list_triangle(triangles)
      }
      
      # Retourner la liste de triangles
      return(triangles)
    }
  )
  
)

# 9).
library(ggplot2)
library(gridExtra)

# Affichage triangle initial et des trois triangle
#' plot_triangles
#'
#' @param triangle liste 
#'
#' @return Deux graphiques (avant et apres)
#' @export

plot_triangles <- function(triangle) {
  A <- triangle[[1]]
  B <- triangle[[2]]
  C <- triangle[[3]]
  
  # Coordonnées du premier triangle
  coords1 <- data.frame(x = c(A[1], B[1], C[1]), y = c(A[2], B[2], C[2]))
  
  # Tracer le premier triangle
  p1 <- ggplot(coords1, aes(x, y)) +
    geom_polygon(colour = "black", fill = "black") +
    coord_equal() +
    ggtitle("Triangle initial")
  
  # Creation fonction divide_triangle_equal
  divide_triangle_equal <- function(A, B, C) {
    # Calculate midpoints
    AB <- midpoint(A, B)
    AC <- midpoint(A, C)
    BC <- midpoint(B, C)
    
    # Construct three new triangles
    triangle1 <- list(A, AB, AC)
    triangle2 <- list(AB, B, BC)
    triangle3 <- list(AC, BC, C)
    
    # Return the new triangles
    return(list(triangle1, triangle2, triangle3))
  }
  
  
  midpoint <- function(p1, p2) {
    return((p1 + p2) / 2)
  }
  
  # Diviser le premier triangle en trois triangles Sierpiński
  triangles <- divide_triangle_equal(A, B, C)
  
  # Coordonnées des trois triangles Sierpiński
  coords2 <- data.frame(x = c(triangles[[1]][[1]][1], triangles[[1]][[2]][1], triangles[[1]][[3]][1],
                              triangles[[2]][[1]][1], triangles[[2]][[2]][1], triangles[[2]][[3]][1],
                              triangles[[3]][[1]][1], triangles[[3]][[2]][1], triangles[[3]][[3]][1]),
                        y = c(triangles[[1]][[1]][2], triangles[[1]][[2]][2], triangles[[1]][[3]][2],
                              triangles[[2]][[1]][2], triangles[[2]][[2]][2], triangles[[2]][[3]][2],
                              triangles[[3]][[1]][2], triangles[[3]][[2]][2], triangles[[3]][[3]][2]))
  
  # Tracer les trois triangles Sierpiński et le triangle du milieu vide
  p2 <- ggplot(coords2, aes(x, y)) +
    geom_polygon(colour = "black", fill = "black") +
    geom_polygon(data = data.frame(x = c((A[1]+B[1])/2, (B[1]+C[1])/2, (C[1]+A[1])/2),
                                   y = c((A[2]+B[2])/2, (B[2]+C[2])/2, (C[2]+A[2])/2)),
                 aes(x, y), colour = "white", fill = "white") +
    coord_equal() +
    ggtitle("Divisé en 4 triangles égaux")
  
  # Afficher les deux graphiques côte à côte
  gridExtra::grid.arrange(p1, p2, ncol = 2)
}

# Exemple
triangle <- list(c(0, 0), c(2, 0), c(1, 2))
plot_triangles(triangle)

# 10). Fait !

# 11).
# Creation d'une fonction pour transformer les coordonnés en longueur
distance_points <- function(point1, point2) {
  sqrt((point2[1] - point1[1])^2 + (point2[2] - point1[2])^2)
}
# On transforme les coordonnés en longueur
A <- triangle[[1]]
B <- triangle[[2]]
C <- triangle[[3]]

a <- distance_points(A, B)
b <- distance_points(B, C)
c <- distance_points(C, A)
# Calcul de l'aire du triangle initial
library(heron)
aire_initial <- heron(a, b, c)
aire_initial
# Calcul de la somme des aires des trois triangles Sierpiński
A <- triangles[[1]][1]
A <- unlist(A)
B <- triangles[[1]][2]
B <- unlist(B)
C <- triangles[[1]][3]
C <- unlist(C)
D <- triangles[[2]][1]
D <- unlist(D)
E <- triangles[[2]][2]
E <- unlist(E)
F <- triangles[[2]][3]
F <- unlist(F)
G <- triangles[[3]][1]
G <- unlist(G)
H <- triangles[[3]][2]
H <- unlist(H)
I <- triangles[[3]][3]
I <- unlist(I)

# longueur des cotes
a <- distance_points(A, B)
b <- distance_points(B, C)
c <- distance_points(A, C)
d <- distance_points(D, E)
e <- distance_points(E, F)
f <- distance_points(D, F)
g <- distance_points(G, H)
h <- distance_points(H, I)
i <- distance_points(G, I)

# Somme des aires
aire1 <- heron(a,b,c)
aire2 <- heron(d,e,f)
aire3 <- heron(g,h,i)
aire_tot <- aire1 + aire2 + aire3
aire_tot

# 12).
library(targets)
tar_read(posts/post-with-code/index.qmd)

# 13). 
library(tarchetypes)
library(quarto)
tar_target(
  "blog",
  tar_quarto(
    "output/blog.html",
    "posts/post-with-code/index.qmd",
    deps = tar_target("targets")
  )
)

# 15). 
tar_make()

