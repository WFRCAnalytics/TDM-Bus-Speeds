# delete-reddit-comments
## Intro: 
This python script uses Selenium and Chromedriver to login and delete user comments from Reddit

## Summary:
Reddit does not allow their users to delete all of their comments at once; they may only delete them one at a time. This tool uses chromedriver to speed up this process. The process isn't perfect, however, repeated runs should yield the desired result. At least until they update the css on their webpage.

## Requirements
- python
- libraries (selenium)
- chromedriver.exe

## Instructions:
1. Install libraries
2. download compatible chromedriver.exe
3. type username (line 1) and password (line 2) into params.text (admittedly, this could be more secure...)
4. adjust number of iterations (line 3) to run
5. run the script repeatedly until all comments are deleted
