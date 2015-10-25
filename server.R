library(shiny)
library(fitdistrplus)
library(data.table)

M <- read.csv("www/Melate.csv")
R <- subset(M, select=c("R1","R2", "R3", "R4", "R5", "R6", "R7"))

shinyServer(function(input, output) {

    output$hist <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- R[,input$select]
        bins <- seq(0, max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, main=paste("Random Variable",input$select),
             breaks=bins,
             prob=TRUE, col = 'lightblue', border = 'white')

        if (input$densBox == TRUE){
            lines(density(x, adjust=2), col="blue", lwd=2)
        }
    })

    output$docs <- renderUI({
        withMathJax(
        helpText('This application plays a lottery game using Mexico\'s Melate'),
        helpText('On left panel are all options you can select.'),
        helpText('1. You can choose between 6 random variables R1-R6 to analyse.'),
        helpText('2. Histogram panel shows the histogram of Random Variable chosen.'),
        helpText('3 Plot Density Curve option shows density curve over hostogram. '),
        helpText('4. You can chose number of bins shown in histogram by slyding bar.'),
        helpText('5. Fitting Distribution optins allow you to select a distribution to haow well it'),

        helpText('6. Models panel show all mathematical formulas of distribution models used.'),
        helpText('7. Fit Panel shows four plots indicating how well a fit is.'),
        helpText('8. Distribution Panel shows a Cullen and Frey graph which indicates how data are distributed.'),
        helpText('9. Data Panel shows a data table with results of each random variable.'),
        helpText('10. Play Panel shows three suggested games.')
        )
    })

        output$models <- renderUI({
        withMathJax(
            helpText('Normal: $$f(x;\\mu,\\lambda) =
                    \\frac{1}{\\sigma \\sqrt{2\\pi}}
                    \\exp^{-\\frac{(x-\\mu)^2}{2\\sigma^2}}$$'),
            helpText('Uniform: $$
                     f(x)=\\begin{cases} \\frac{1}{b - a} & \\mathrm{for}\\ a \\le x
                     \\le b,\\ 0 & \\mathrm{for}\\ x<a\\ \\mathrm{or}\\ x>b
                     \\end{cases}$$ '),
            helpText('Gamma:
                     $$f(x) = \\lambda e^{-\\lambda x} \\frac{(\\lambda x)^{k-1}}
                     {\\Gamma(k)} $$'),
            helpText('Beta: $$f(x) = \\frac{\\Gamma(a+b)}{\\Gamma(a)\\Gamma(b)}
                     x^{a-1}(1-x)^{b-1} $$'),
            helpText('Logistic: $$F(x; \\mu,s) = \\frac{1}{1+e^{-(x-\\mu)/s}}$$'),
            helpText('Exponential: $$
                     f(x)= P(x)=\\left\\{\\begin{matrix} \\lambda e^{-\\lambda x}
                    & \\quad \\text{for } x \\ge 0, \\ 0 \\text{ on the contrary}
                    \\end{matrix}\\right.
                     $$'),
            helpText('Log-normal: $$f(x;\\mu,\\sigma) = \\frac{1}{x \\sigma
                     \\sqrt{2 \\pi}} e^{-(\\ln(x) - \\mu)^2/2\\sigma^2} $$'),
            helpText('Weibull: $$ f(x;\\lambda,k) = \\begin{cases}
                     \\frac{k}{\\lambda}\\left(\frac{x}{\\lambda}\\right)^{k-1}
                     e^{-(x/\\lambda)^{k}} & x\\geq 0,\\ 0 x<0\\end{cases}$$')
        )})



    output$fit <- renderPlot({
        x    <- R[,input$select]
        # <- x/(max(x) + 0.0001)
        #fit <- fitdist(x, distr=input$dist)
        ##print(fit)
        ##fit$aic
        #plot(fit)
        ##title(main=expression(paste("Distribution ",beta)))
        #title(main=input$dist)
        fw <- fitdist(x, distr="weibull")
        fg <- fitdist(x, distr="gamma")
        fl <- fitdist(x, distr="lnorm")
        par(mfrow = c(2, 2))
        plot.legend <- c("Weibull", "lognormal", "gamma")
        denscomp(list(fw, fl, fg), legendtext = plot.legend)
        qqcomp(list(fw, fl, fg), legendtext = plot.legend)
        cdfcomp(list(fw, fl, fg), legendtext = plot.legend)
        ppcomp(list(fw, fl, fg), legendtext = plot.legend)
    })

    output$dist <- renderPlot({
        x    <- R[,input$select]
        descdist(x, discrete = FALSE, boot=1000, boot.col="yellow")
    })

    output$table <- renderDataTable({
        data.frame(R)
    })

    output$play <- renderDataTable({
        set.seed( as.integer((as.double(Sys.time())*1000+Sys.getpid()) %% 2^31))
        gt <- data.table(c(0,0,0,0,0,0),c(0,0,0,0,0,0),
                         c(0,0,0,0,0,0))
        setnames(gt,c("Game 1","Game 2","Game 3"))
        j <- 1
        for(r in c("R1","R2","R3","R4","R5","R6")){
            x <- R[,r]
            fit <- fitdist(x, "norm")
            n <- round(rnorm(3,mean = fit$estimate["mean"],
                             sd = fit$estimate["sd"]))
            for(i in 1:3) gt[j,i] <- n[i]
            j <- j + 1
        }
        gt
    })
})
