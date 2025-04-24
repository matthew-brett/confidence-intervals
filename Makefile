slides: slides/confidence_intervals.qmd
	quarto render slides --output-dir ../_site

clean:
	git clean -fxd
