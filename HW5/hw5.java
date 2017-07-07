/* Name: Jaekwan Ahn

   UID: 604057669

   Others With Whom I Discussed Things: None

   Other Resources I Consulted: Discussion Slides, Piazza and Lecture Notes
   
*/

import java.io.*;
//headers included
import java.util.Arrays;
import java.lang.Math;
import java.util.concurrent.*;
import java.util.stream.IntStream;

// a marker for code that you need to implement
class ImplementMe extends RuntimeException {}

// an RGB triple
class RGB {
    public int R, G, B;

    RGB(int r, int g, int b) {
    	R = r;
		G = g;
		B = b;
    }

    public String toString() { return "(" + R + "," + G + "," + B + ")"; }

}


// an object representing a single PPM image
class PPMImage {
    protected int width, height, maxColorVal;
    protected RGB[] pixels;

    public PPMImage(int w, int h, int m, RGB[] p) {
		width = w;
		height = h;
		maxColorVal = m;
		pixels = p;
    }

    // parse a PPM image file named fname and produce a new PPMImage object
    public PPMImage(String fname) 
    	throws FileNotFoundException, IOException {
		FileInputStream is = new FileInputStream(fname);
		BufferedReader br = new BufferedReader(new InputStreamReader(is));
		br.readLine(); // read the P6
		String[] dims = br.readLine().split(" "); // read width and height
		int width = Integer.parseInt(dims[0]);
		int height = Integer.parseInt(dims[1]);
		int max = Integer.parseInt(br.readLine()); // read max color value
		br.close();

		is = new FileInputStream(fname);
	    // skip the first three lines
		int newlines = 0;
		while (newlines < 3) {
	    	int b = is.read();
	    	if (b == 10)
				newlines++;
		}

		int MASK = 0xff;
		int numpixels = width * height;
		byte[] bytes = new byte[numpixels * 3];
        is.read(bytes);
		RGB[] pixels = new RGB[numpixels];
		for (int i = 0; i < numpixels; i++) {
	    	int offset = i * 3;
	    	pixels[i] = new RGB(bytes[offset] & MASK, 
	    						bytes[offset+1] & MASK, 
	    						bytes[offset+2] & MASK);
		}
		is.close();

		this.width = width;
		this.height = height;
		this.maxColorVal = max;
		this.pixels = pixels;
    }

	// write a PPMImage object to a file named fname
    public void toFile(String fname) throws IOException {
		FileOutputStream os = new FileOutputStream(fname);

		String header = "P6\n" + width + " " + height + "\n" 
						+ maxColorVal + "\n";
		os.write(header.getBytes());

		int numpixels = width * height;
		byte[] bytes = new byte[numpixels * 3];
		int i = 0;
		for (RGB rgb : pixels) {
	    	bytes[i] = (byte) rgb.R;
	    	bytes[i+1] = (byte) rgb.G;
	    	bytes[i+2] = (byte) rgb.B;
	    	i += 3;
		}
		os.write(bytes);
		os.close();
    }

	// implement using Java 8 Streams
    public PPMImage negate() {
    	RGB[] negatedPixels = Arrays.stream(pixels).parallel()		
							  .map(pixel -> {
								  RGB negatedPixel = new RGB(this.maxColorVal - pixel.R, this.maxColorVal - pixel.G, this.maxColorVal - pixel.B);
								  return negatedPixel; 
							  })
							  .toArray(RGB[]::new);
		PPMImage negated = new PPMImage(this.width, this.height, this.maxColorVal, negatedPixels);
		return negated;
    }

	// implement using Java 8 Streams
    public PPMImage greyscale() {
    	RGB[] greyPixels = Arrays.stream(pixels).parallel()		
							  .map(pixel -> { //keep them as doubles, then call the rounding function that converts a double to a long and then finally cast the long to an int
							  	  double calculated = pixel.R * 0.299 + pixel.G * 0.587 + pixel.B * 0.114;
							  	  long newVal1 = Math.round(calculated);
							  	  int newVal = (int) newVal1; 
								  RGB greyPixel = new RGB(newVal, newVal, newVal);
								  return greyPixel; 
							  })
							  .toArray(RGB[]::new);
		PPMImage greyscaled = new PPMImage(this.width, this.height, this.maxColorVal, greyPixels);
		return greyscaled;
    }    
    
	// implement using Java's Fork/Join library
    public PPMImage mirrorImage() {
    	RGB[] mirroredPixels = new RGB[this.width * this.height];
    	MirrorImage mir = new MirrorImage(this.width, 0, this.height - 1, pixels, mirroredPixels);
    	mir.compute();
    	PPMImage mirrored = new PPMImage(this.width, this.height, this.maxColorVal, mirroredPixels);
    	return mirrored;
    }

	// implement using Java 8 Streams
    public PPMImage mirrorImage2() {
    	RGB[] mirroredPixels = IntStream.range(0, this.width * this.height).parallel()		
							  .mapToObj(index -> {
							  	  int remain = index % width;
							  	  RGB mirroredPixel = pixels[index - remain + width - 1 - remain];
								  return mirroredPixel; 
							  })
							  .toArray(RGB[]::new);
		PPMImage mirrored2 = new PPMImage(this.width, this.height, this.maxColorVal, mirroredPixels);
		return mirrored2;
    }

	// implement using Java's Fork/Join library
    public PPMImage gaussianBlur(int radius, double sigma) {
		RGB[] blurredPixels = new RGB[this.width * this.height];
    	Gaussian g = new Gaussian();
    	BlurImage blur = new BlurImage(this.width, this.height, 0, this.height - 1, radius, pixels, blurredPixels, g.gaussianFilter(radius, sigma));
    	blur.compute();
    	PPMImage blurred = new PPMImage(this.width, this.height, this.maxColorVal, blurredPixels);
    	return blurred;
    }
}

class MirrorImage extends RecursiveAction {
	static final int SEQUENTIAL_THRESHOLD = 10;
	protected int width, low, high, height;
    protected RGB[] pixels, mirrored;

	MirrorImage(int width, int low, int high, RGB[] pixels, RGB[] mirrored){
		this.width = width;
		this.low = low;
		this.high = high;
		this.height = high - low + 1;
		this.pixels = pixels;
		this.mirrored = mirrored;
	}

	public void compute(){
		if(this.height <= SEQUENTIAL_THRESHOLD){
			for(int i = this.low; i <= this.high; i++){
				int k = this.width - 1;
				for(int j = 0; j < this.width; j++, k--){ //update mirrored RGB
					mirrored[i * this.width + j] = pixels[i * this.width + k];
				}
			}
			return;
		} else{
			int mid;
			if(this.height % 2 == 0){
				mid = (this.low + this.high - 1) / 2;
			} else{
				mid = (this.low + this.high) / 2;
			}
			MirrorImage first = new MirrorImage(this.width, this.low, mid, pixels, mirrored);
			MirrorImage second = new MirrorImage(this.width, mid + 1, this.high, pixels, mirrored);
			second.fork();
			first.compute();
			second.join();
			return;
		}
	}
}

class BlurImage extends RecursiveAction {
	static final int SEQUENTIAL_THRESHOLD = 10;
	protected int width, low, high, height, origHeight, radius;
    protected RGB[] pixels, blurred;
    protected double[][] gaussian_filter;

	BlurImage(int width, int origHeight, int low, int high, int radius, RGB[] pixels, RGB[] blurred, double[][] gaussian_filter){
		this.width = width;
		this.origHeight = origHeight;
		this.low = low;
		this.high = high;
		this.height = high - low + 1;
		this.radius = radius;
		this.pixels = pixels;
		this.blurred = blurred;
		this.gaussian_filter = gaussian_filter;
	}

	public void compute(){
		if(this.height <= SEQUENTIAL_THRESHOLD){
			for(int i = this.low; i <= this.high; i++){
				for(int j = 0; j < width; j++){
					double curR = 0.0;
					double curG = 0.0;
					double curB = 0.0;
					for(int k = 0; k < this.gaussian_filter.length; k++){
						for(int l = 0; l < this.gaussian_filter.length; l++){
							int r = i - this.radius + k;
							//extreme cases
							if(r < 0){
								r = 0;
							} else if(r >= this.origHeight){
								r = this.origHeight - 1;
							}

							int c = j - this.radius + l;
							//extreme cases
							if(c < 0){
								c = 0;
							} else if(c >= this.width){
								c = this.width - 1;
							}

							//update
							int index = width * r + c;
							curR = this.gaussian_filter[k][l] * pixels[index].R + curR;
							curG = this.gaussian_filter[k][l] * pixels[index].G + curG;
							curB = this.gaussian_filter[k][l] * pixels[index].B + curB;
						}
					}
					//round and cast then update new RGB
					long newR1 = Math.round(curR);
					long newG1 = Math.round(curG);
					long newB1 = Math.round(curB);
					int newR = (int) newR1;
					int newG = (int) newG1;
					int newB = (int) newB1;
					RGB blur = new RGB(newR, newG, newB);
					blurred[i * this.width + j] = blur;//new RGB(newR, newG, newB); 
				}
			}
			return;
		} else{
			int mid;
			if(this.height % 2 == 0){
				mid = (this.low + this.high - 1) / 2;
			} else{
				mid = (this.low + this.high) / 2;
			}
			BlurImage first = new BlurImage(this.width, this.origHeight, this.low, mid, this.radius, pixels, blurred, this.gaussian_filter);
			BlurImage second = new BlurImage(this.width, this.origHeight, mid + 1, this.high, this.radius, pixels, blurred, this.gaussian_filter);
			second.fork();
			first.compute();
			second.join();
			return;
		}
	}
}

// code for creating a Gaussian filter
class Gaussian {

    protected static double gaussian(int x, int mu, double sigma) {
		return Math.exp( -(Math.pow((x-mu)/sigma,2.0))/2.0 );
    }

    public static double[][] gaussianFilter(int radius, double sigma) {
		int length = 2 * radius + 1;
		double[] hkernel = new double[length];
		for(int i=0; i < length; i++)
	    	hkernel[i] = gaussian(i, radius, sigma);
		double[][] kernel2d = new double[length][length];
		double kernelsum = 0.0;
		for(int i=0; i < length; i++) {
	    	for(int j=0; j < length; j++) {
				double elem = hkernel[i] * hkernel[j];
				kernelsum += elem;
				kernel2d[i][j] = elem;
	    	}
		}
		for(int i=0; i < length; i++) {
	    	for(int j=0; j < length; j++)
				kernel2d[i][j] /= kernelsum;
		}
		return kernel2d;
    }
}

//Test
class Test {
	public static void main(String[] args) throws Exception {
		PPMImage origin = new PPMImage("florence.ppm");

		PPMImage negated = origin.negate();
		negated.toFile("negated.ppm");

		PPMImage greyscaled = origin.greyscale();
		greyscaled.toFile("greyscaled.ppm");

		PPMImage mirrored = origin.mirrorImage();
		mirrored.toFile("mirrored.ppm");
		
		mirrored = origin.mirrorImage2();
		mirrored.toFile("mirrored2.ppm");
		
		origin.gaussianBlur(1, 2.0).toFile("blurred.ppm");
	}
}


