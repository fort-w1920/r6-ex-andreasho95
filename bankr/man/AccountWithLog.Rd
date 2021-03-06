% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/account-r6.R
\name{AccountWithLog}
\alias{AccountWithLog}
\title{R6 Object representing a bank account with transaction log}
\description{
Implements an R6 structure for a bank account with transaction log and corresponding actions.
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{balance}}{Provides access to account balance.}

\item{\code{trans_log}}{Provides access to current transition log.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{AccountWithLog$new()}}
\item \href{#method-withdraw}{\code{AccountWithLog$withdraw()}}
\item \href{#method-deposit}{\code{AccountWithLog$deposit()}}
\item \href{#method-print_bank_statement}{\code{AccountWithLog$print_bank_statement()}}
\item \href{#method-clone}{\code{AccountWithLog$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\subsection{Method \code{new()}}{
Create a new AccountWithLog object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AccountWithLog$new(balance = 0, trans_log = TransactionLog$new())}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{balance}}{Initial balance of account.}

\item{\code{trans_log}}{Transition log for the account. Requires a 'TransactionLog' object.
On default it will create a fresh log for the new account.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new `AccountWithLog`.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-withdraw"></a>}}
\subsection{Method \code{withdraw()}}{
Withdraw money from bank account.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AccountWithLog$withdraw(amount)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{amount}}{Amount to be withdrawn. Must be a whole number.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-deposit"></a>}}
\subsection{Method \code{deposit()}}{
Deposit money into bank account.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AccountWithLog$deposit(amount)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{amount}}{Amount to be deposited. Must be a whole number.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-print_bank_statement"></a>}}
\subsection{Method \code{print_bank_statement()}}{
Printes the current bank statement in a nice format including a header.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AccountWithLog$print_bank_statement()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AccountWithLog$clone()}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
