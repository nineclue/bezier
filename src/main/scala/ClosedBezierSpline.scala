object ClosedBezierSpline {
    def apply(knots: Seq[Point]) = ???
}

/* 
 * https://www.codeproject.com/Articles/33776/Draw-Closed-Smooth-Curve-with-Bezier-Spline
/// <summary>
/// Closed Bezier Spline Control Points calculation.
/// </summary>
public static class ClosedBezierSpline
{
	/// <summary>
	/// Get Closed Bezier Spline Control Points.
	/// </summary>
	/// <param name="knots">Input Knot Bezier spline points.</param>
	/// <param name="firstControlPoints">
	/// Output First Control points array of the same 
	/// length as the <paramref name="knots"> array.</param>
	/// <param name="secondControlPoints">
	/// Output Second Control points array of the same
	/// length as the <paramref name="knots"> array.</param>
	public static void GetCurveControlPoints(Point[] knots, 
		out Point[] firstControlPoints, out Point[] secondControlPoints)
	{
		int n = knots.Length;
		if (n <= 2)
		{ // There should be at least 3 knots to draw closed curve.
			firstControlPoints = new Point[0];
			secondControlPoints = new Point[0];
			return;
		}

		// Calculate first Bezier control points

		// The matrix.
		double[] a = new double[n], b = new double[n], c = new double[n];
		for (int i = 0; i < n; ++i)
		{
			a[i] = 1;
			b[i] = 4;
			c[i] = 1;
		}

		// Right hand side vector for points X coordinates.
		double[] rhs = new double[n];
		for (int i = 0; i < n; ++i)
		{
			int j = (i == n - 1) ? 0 : i + 1;
			rhs[i] = 4 * knots[i].X + 2 * knots[j].X;
		}
		// Solve the system for X.
		double[] x = Cyclic.Solve(a, b, c, 1, 1, rhs);

		// Right hand side vector for points Y coordinates.
		for (int i = 0; i < n; ++i)
		{
			int j = (i == n - 1) ? 0 : i + 1;
			rhs[i] = 4 * knots[i].Y + 2 * knots[j].Y;
		}
		// Solve the system for Y.
		double[] y = Cyclic.Solve(a, b, c, 1, 1, rhs);

		// Fill output arrays.
		firstControlPoints = new Point[n];
		secondControlPoints = new Point[n];
		for (int i = 0; i < n; ++i)
		{
			// First control point.
			firstControlPoints[i] = new Point(x[i], y[i]);
			// Second control point.
			secondControlPoints[i] = new Point
				(2 * knots[i].X - x[i], 2 * knots[i].Y - y[i]);
		}
	}
}

/// <summary>
/// Solves the cyclic set of linear equations.
/// </summary>
/// <remarks>
/// The cyclic set of equations have the form
/// ---------------------------
/// b0 c0  0 · · · · · · ß
///	a1 b1 c1 · · · · · · ·
///  · · · · · · · · · · · 
///  · · · a[n-2] b[n-2] c[n-2]
/// a  · · · · 0  a[n-1] b[n-1]
/// ---------------------------
/// This is a tridiagonal system, except for the matrix elements 
/// a and ß in the corners.
/// </remarks>
public static class Cyclic
{
	/// <summary>
	/// Solves the cyclic set of linear equations. 
	/// </summary>
	/// <remarks>
	/// All vectors have size of n although some elements are not used.
	/// The input is not modified.
	/// </remarks>
	/// <param name="a">Lower diagonal vector of size n; a[0] not used.</param>
	/// <param name="b">Main diagonal vector of size n.</param>
	/// <param name="c">Upper diagonal vector of size n; c[n-1] not used.</param>
	/// <param name="alpha">Bottom-left corner value.</param>
	/// <param name="beta">Top-right corner value.</param>
	/// <param name="rhs">Right hand side vector.</param>
	/// <returns>The solution vector of size n.</returns>
    	public static double[] Solve(double[] a, double[] b, 
		double[] c, double alpha, double beta, double[] rhs)
	{
		// a, b, c and rhs vectors must have the same size.
		if (a.Length != b.Length || c.Length != b.Length || 
						rhs.Length != b.Length)
			throw new ArgumentException
			("Diagonal and rhs vectors must have the same size.");
		int n = b.Length;
		if (n <= 2) 
			throw new ArgumentException
			("n too small in Cyclic; must be greater than 2.");

		double gamma = -b[0]; // Avoid subtraction error in forming bb[0].
		// Set up the diagonal of the modified tridiagonal system.
		double[] bb = new Double[n];
		bb[0] = b[0] - gamma;
		bb[n-1] = b[n - 1] - alpha * beta / gamma;
		for (int i = 1; i < n - 1; ++i)
			bb[i] = b[i];
		// Solve A · x = rhs.
		double[] solution = Tridiagonal.Solve(a, bb, c, rhs);
		double[] x = new Double[n];
		for (int k = 0; k < n; ++k)
			x[k] = solution[k];

		// Set up the vector u.
		double[] u = new Double[n];
		u[0] = gamma;
		u[n-1] = alpha;
		for (int i = 1; i < n - 1; ++i) 
			u[i] = 0.0;
		// Solve A · z = u.
		solution = Tridiagonal.Solve(a, bb, c, u);
		double[] z = new Double[n];
		for (int k = 0; k < n; ++k)
			z[k] = solution[k];

		// Form v · x/(1 + v · z).
		double fact = (x[0] + beta * x[n - 1] / gamma)
			/ (1.0 + z[0] + beta * z[n - 1] / gamma);

		// Now get the solution vector x.
		for (int i = 0; i < n; ++i) 
			x[i] -= fact * z[i];
		return x;
	} 
}

/// <summary>
/// Tridiagonal system solution.
/// </summary>
public static class Tridiagonal
{
	/// <summary>
	/// Solves a tridiagonal system.
	/// </summary>
	/// <remarks>
	/// All vectors have size of n although some elements are not used.
	/// </remarks>
	/// <param name="a">Lower diagonal vector; a[0] not used.</param>
	/// <param name="b">Main diagonal vector.</param>
	/// <param name="c">Upper diagonal vector; c[n-1] not used.</param>
	/// <param name="rhs">Right hand side vector</param>
	/// <returns>system solution vector</returns>
	public static double[] Solve(double[] a, double[] b, double[] c, double[] rhs)
	{
		// a, b, c and rhs vectors must have the same size.
		if (a.Length != b.Length || c.Length != b.Length || 
						rhs.Length != b.Length)
			throw new ArgumentException
			("Diagonal and rhs vectors must have the same size.");
		if (b[0] == 0.0)
			throw new InvalidOperationException("Singular matrix.");
        		// If this happens then you should rewrite your equations as a set of 
		// order N - 1, with u2 trivially eliminated.

		ulong n = Convert.ToUInt64(rhs.Length);
		double[] u = new Double[n];
		double[] gam = new Double[n]; 	// One vector of workspace, 
					  	// gam is needed.
		
		double bet = b[0];
		u[0] = rhs[0] / bet;
		for (ulong j = 1;j < n;j++) // Decomposition and forward substitution.
		{
		   	gam[j] = c[j-1] / bet;
            		bet = b[j] - a[j] * gam[j];
			if (bet == 0.0)  
				// Algorithm fails.
				throw new InvalidOperationException
							("Singular matrix.");
		         u[j] = (rhs[j] - a[j] * u[j - 1]) / bet;
        		}
        		for (ulong j = 1;j < n;j++) 
			u[n - j - 1] -= gam[n - j] * u[n - j]; // Backsubstitution.

		return u;
	}
}
*/