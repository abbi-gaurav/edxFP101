# Use the existing Haskell image as our base
FROM haskell:8.2.2

# Checkout our code onto the Docker container
WORKDIR /app
ADD . /app

# Build and test our code, then install the “scotty-app-exe” executable
RUN stack setup
RUN stack build --copy-bins

# Expose a port to run our application
EXPOSE 3000

# Run the server command
CMD scotty-app-exe