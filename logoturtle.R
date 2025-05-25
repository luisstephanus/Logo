

turtle <- new.env()

init_turtle <- function(xlim = c(-100, 100), ylim = c(-100, 100)) {
  # Set initial state
  turtle$position <- c(0, 0)
  turtle$heading <- 90  # Degrees, 90 = facing "up" like in LOGO
  turtle$pen <- TRUE     # Pen down = drawing
  turtle$visible <- TRUE
  turtle$history <- list()
  turtle$xlim <- xlim
  turtle$ylim <- ylim
  
  # Initialize the plot (canvas)
  plot(NA, xlim = xlim, ylim = ylim, type = "n", asp = 1,
       xlab = "", ylab = "", axes = FALSE, main = "Turtle Graphics in R")
  
  # Draw the turtle at starting point
  draw_turtle()
}

draw_turtle <- function() {
  if (!turtle$visible) return()
  
  # Extract state
  x <- turtle$position[1]
  y <- turtle$position[2]
  angle <- turtle$heading
  
  # Triangle shape
  size <- 10  # Size of the turtle
  radians <- function(deg) deg * pi / 180
  
  # Calculate triangle points
  p1 <- c(x, y)
  p2 <- c(x + size * cos(radians(angle - 135)),
          y + size * sin(radians(angle - 135)))
  p3 <- c(x + size * cos(radians(angle + 135)),
          y + size * sin(radians(angle + 135)))
  
  # Draw triangle
  polygon(x = c(p1[1], p2[1], p3[1]), y = c(p1[2], p2[2], p3[2]),
          col = "darkgreen", border = "black")
}


## sapply optimized the code... because it now draws all segments at once and not with for loop, one at a time.
redraw_scene <- function() {
  plot(NA, xlim = turtle$xlim, ylim = turtle$ylim, type = "n", asp = 1,
       xlab = "", ylab = "", axes = FALSE, main = "Turtle Graphics in R")
  
  if (length(turtle$history) > 0) {
    segments(
      x0 = sapply(turtle$history, `[`, 1),
      y0 = sapply(turtle$history, `[`, 2),
      x1 = sapply(turtle$history, `[`, 3),
      y1 = sapply(turtle$history, `[`, 4)
    )
  }
  
  draw_turtle()
}

move_turtle <- function(steps, redraw = TRUE) {
  x0 <- turtle$position[1]
  y0 <- turtle$position[2]
  angle_rad <- turtle$heading * pi / 180
  
  x1 <- x0 + steps * cos(angle_rad)
  y1 <- y0 + steps * sin(angle_rad)
  
  if (turtle$pen) {
    turtle$history[[length(turtle$history) + 1]] <- c(x0, y0, x1, y1)
  }
  
  turtle$position <- c(x1, y1)
  
  if (redraw) redraw_scene()
}

fd <- function(steps, redraw = TRUE) move_turtle(steps, redraw = redraw)
bk <- function(steps, redraw = TRUE) move_turtle(-steps, redraw = redraw)



lt <- function(degrees, redraw = TRUE) {
  turtle$heading <- (turtle$heading + degrees) %% 360
  if (redraw) redraw_scene()
}

rt <- function(degrees, redraw = TRUE) {
  turtle$heading <- (turtle$heading - degrees) %% 360
  if (redraw) redraw_scene()
}


home <- function() {
  x0 <- turtle$position[1]
  y0 <- turtle$position[2]
  x1 <- 0
  y1 <- 0
  
  if (turtle$pen) {
    turtle$history[[length(turtle$history) + 1]] <- c(x0, y0, x1, y1)
  }
  turtle$position <- c(x1, y1)
  
  redraw()
}

clearscreen <- function() {
  
  turtle$history <- list()
  
  turtle$position <- turtle$home
  turtle$heading <- 90
  
  redraw_scene()
}

## final command:

for (i in 1:20){
  for (j in 1:180) {
    fd(1, redraw = FALSE)
    rt(2, redraw = FALSE)
  }
  rt(18, redraw = FALSE)
}

