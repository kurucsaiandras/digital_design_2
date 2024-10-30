import cv2
import numpy as np
import matplotlib.pyplot as plt

# Load the PGM image in grayscale
image_path = 'pic1.pgm'
img = cv2.imread(image_path, cv2.IMREAD_GRAYSCALE)

# Apply the Sobel operator
sobel_x = cv2.Sobel(img, cv2.CV_64F, 1, 0, ksize=3)  # Sobel operator in the x direction
sobel_y = cv2.Sobel(img, cv2.CV_64F, 0, 1, ksize=3)  # Sobel operator in the y direction

# Calculate the resulting pixel values
result = (np.abs(sobel_x) + np.abs(sobel_y)) / 6
result = np.uint8(result[1:,:])  # convert to uint8

# Set the number of rows (N) for printing in Nx4 format
N = 100

# Flatten the result array and convert each pixel to hexadecimal
hex_pixels = [f"{pixel:02X}" for pixel in result.flatten()]

# Print the pixels in groups of 4
for i in range(0, N, 4):
    print(", ".join(hex_pixels[i:i+4][::-1]))

# Display the images
plt.figure(figsize=(10, 6))

plt.subplot(1, 3, 1)
plt.title('Original Image')
plt.imshow(img, cmap='gray')

plt.subplot(1, 3, 2)
plt.title('Sobel X')
plt.imshow(sobel_x, cmap='gray')

plt.subplot(1, 3, 3)
plt.title('Sobel Y')
plt.imshow(sobel_y, cmap='gray')

plt.figure(figsize=(5, 5))
plt.title('Resulting Image')
plt.imshow(result, cmap='gray')

plt.show()

