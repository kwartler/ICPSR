# ICPSR

This repo supports ICPSR NLP & Text Mining 2024 Summer Topic Workshop.

## Software requirements

This course requires [R](https://cran.r-project.org/), [R-studio](https://posit.co/download/rstudio-desktop/) both freely available.

Additionally on day 3, we will explore programmatic requests to "local LLMs". This requires [LM-Studio](https://lmstudio.ai/). However students with Intel Macs cannot use this software. Only Mac M1,2,3, Windows and Linux machines are supported. Students with this issue should install [Ollama](https://ollama.com/download) as an alternative. **If you use Ollama instead of lm-studio, you will have to adjust our class code based on the example below and possibly install a user interface such as [webui](https://github.com/open-webui/open-webui)**

*Students with older computers, old GPUs or a small amount of RAM, may not be able to execute this portion of the lesson*

For students needing Ollama (less preferred), download it then perform the tasks at the end of this readme.

## Lessons

### Day1:

-   R Setup & Logistics
-   What is NLP, git, r syntax, r-studio?
-   Preprocessing steps: string manipulation, term frequency, Chapter 1

### Day2:

-   Bag of Words DTM/TDM
-   Visualizations: wordclouds, histograms, pyramid plots, word networks, dendrograms, associations, dendrograms - “Homework” HW1-Basics of R Coding

### Day3:

-   Basic sentiment analysis with lexicons
-   Basic document clustering
-   LLM Basics, prompting & (time permitting) prompt chains/agentic workflows

### Packages for R

R is customized for specific functions using libraries or packages.  In this class we will use the following packages.  Once you have R and R studio installed run the following command in your console.  Don't worry if you struggle, on day 1 we will set aside time to help though we aren't performing technical support.

```
# Install library pacman
install.packages('pacman')

# Use pacman to install other libraries)
pacman::p_load(dplyr, ggplot2, ggthemes, qdapRegex, slam, stringi, stringr, tm)

```


#### For students unable to use LM Studio here are some set up and testing instructions for Ollama.

1. In terminal

`run ollama`

2.  Install a small llm for testing, takes a few minutes.

`ollama run gemma:2b`

3.  You will see a prompt in your terminal like this. You can ask a simple question, "What is the capital of France?" in the terminal.

`>>> Send a message (/? for help)`

`>>> What is the captial of France?`

4.  Next, let's perform a programmatic request while Ollama is running. Open R and try this code. If you get a response in R, your instance is working as intended. **You will have to adjust our class code to fit this example API request which is slightly different**.  For additional help, this is a great [site](https://curlconverter.com/r/) to help convert CURL requests to multiple languages.

```         
# Libraries
library(httr)
library(jsonlite)

# Inputs
prompt <- "What is the capital of France?" 

# API call inputs
headers <- c(`Content-Type` = "application/json") 
data    <- list(model = "gemma:2b", # Be sure to change to the model name you're using
                prompt = prompt)

# API Request
res <- httr::POST(
  url = "http://localhost:11434/api/generate", 
  httr::add_headers(.headers=headers), 
  body = jsonlite::toJSON(data, auto_unbox = TRUE), 
  encode = "json")

# Parse the streaming in JSON
llmResponse <- httr::content(res,as = "text", encoding = "UTF-8")
llmResponse <- strsplit(llmResponse, "\n")[[1]]
llmResponse <- lapply(llmResponse, fromJSON)
llmResponse <- paste(unlist(lapply(llmResponse, '[', 'response')), collapse = '')
llmResponse
```

5. To exit Ollama in terminal run this command.  The API will still be running so you could still execute step 4.

`>>> /bye`

6. To stop Ollama in terminal run this command.  The in the upper toolbar, there is a llama icon.  You have to click the icon and stop running it

`brew services stop ollama`

If that doesn't work try running in terminal.  This will return a number.  

`$ pgrep ollama` 

To kill that process take the number presented and use this command in terminal.

`kill 74877`

7. To fully uninstall and remove Ollama use this [site](https://collabnix.com/how-to-uninstall-ollama/#Stop_the_Ollama_Service), though commands for mac are slightly different and are cited [here](https://github.com/ollama/ollama/issues/2028).
