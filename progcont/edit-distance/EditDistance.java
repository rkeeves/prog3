import java.util.stream.IntStream;

class EditDistance{
	
	static int min(int[] arr){
		return IntStream.of(arr).min().orElse(Integer.MAX_VALUE);
	}
	
	static int levenshteinDistance(String s, String t){
		int m = s.length();
		int n = t.length();
		int d[][] = new int[m+1][];
		for(int i = 0; i<m+1; i++){
			d[i] = new int[n+1];
			for(int j = 0; j<n+1; j++){
				d[i][j] = 0;
			}
		}
		
		for(int i = 1; i<m+1; i++){
			d[i][0] = i;
		}
		
		for(int j = 1; j<n+1; j++){
			d[0][j] = j;
		}
		
		int substitutionCost;
		for(int j = 1; j<n+1; j++){
			for(int i = 1; i<m+1; i++){
				if(s.charAt(i-1) == t.charAt(j-1))
					substitutionCost = 0;
				else
					substitutionCost = 1;
				d[i][j] = min(new int[]{d[i-1][j] + 1,
				d[i][j-1] + 1,
				d[i-1][j-1] + substitutionCost});
			}
		}
		return d[m][n];
	}
  
	public static void main(String[] args){
		System.out.println(levenshteinDistance("Saturday","Sunday"));
	}
}