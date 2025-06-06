% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Record.R
\name{Record}
\alias{Record}
\title{Record Class}
\description{
The Record class represents a single row in a database table. It provides methods
for creating, updating, and deleting individual records.
}
\details{
Record is an R6 class that works in conjunction with the TableModel class. Each Record
instance corresponds to a single row in the database table represented by its associated
TableModel. The class provides methods for CRUD (Create, Read, Update, Delete) operations
on individual records.
}
\section{Methods}{

\describe{
  \item{\code{initialize(model, data = list())}}{Constructor for creating a new Record instance.}
  \item{\code{create()}}{Inserts this record into the database.}
  \item{\code{update()}}{Updates this record in the database.}
  \item{\code{delete()}}{Deletes this record from the database.}
}
}

\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{model}}{A TableModel object. Represents the database table this record belongs to.}

\item{\code{data}}{A list. Contains the data for this record, with column names as keys.}

\item{\code{relationships}}{A list. Contains the relationships defined for this record's model.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-Record-new}{\code{Record$new()}}
\item \href{#method-Record-create}{\code{Record$create()}}
\item \href{#method-Record-update}{\code{Record$update()}}
\item \href{#method-Record-delete}{\code{Record$delete()}}
\item \href{#method-Record-refresh}{\code{Record$refresh()}}
\item \href{#method-Record-relationship}{\code{Record$relationship()}}
\item \href{#method-Record-print}{\code{Record$print()}}
\item \href{#method-Record-clone}{\code{Record$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Record-new"></a>}}
\if{latex}{\out{\hypertarget{method-Record-new}{}}}
\subsection{Method \code{new()}}{
Initialize a new Record instance.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Record$new(model, ..., .data = list())}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{model}}{A TableModel object representing the database table.}

\item{\code{...}}{Named arguments corresponding to field values for the record.}

\item{\code{.data}}{A named list of field values (alternative to ...).}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new Record instance.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Record-create"></a>}}
\if{latex}{\out{\hypertarget{method-Record-create}{}}}
\subsection{Method \code{create()}}{
Insert this record into the database.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Record$create()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Invisible NULL
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Record-update"></a>}}
\if{latex}{\out{\hypertarget{method-Record-update}{}}}
\subsection{Method \code{update()}}{
Update this record in the database.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Record$update(..., .data = list())}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Named arguments corresponding to field values to update.}

\item{\code{.data}}{A named list of field values to update (alternative to ...).}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The Record instance (invisibly).
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Record-delete"></a>}}
\if{latex}{\out{\hypertarget{method-Record-delete}{}}}
\subsection{Method \code{delete()}}{
Delete this record from the database.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Record$delete()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Invisible NULL
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Record-refresh"></a>}}
\if{latex}{\out{\hypertarget{method-Record-refresh}{}}}
\subsection{Method \code{refresh()}}{
Refresh this record from the database.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Record$refresh()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
The Record instance (invisibly).
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Record-relationship"></a>}}
\if{latex}{\out{\hypertarget{method-Record-relationship}{}}}
\subsection{Method \code{relationship()}}{
Retrieve related records based on a defined relationship.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Record$relationship(rel_name, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{rel_name}}{The name of the relationship to query.}

\item{\code{...}}{Additional arguments passed to the related model's read method.}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
This method returns related records based on the relationship type:
- For 'one_to_one' and 'many_to_one' relationships, it returns a single Record object or NULL.
- For 'one_to_many' and 'many_to_many' relationships, it returns a list of Record objects.
}

\subsection{Returns}{
A single Record, a list of Records, or NULL, depending on the relationship type.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Record-print"></a>}}
\if{latex}{\out{\hypertarget{method-Record-print}{}}}
\subsection{Method \code{print()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Record$print()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Record-clone"></a>}}
\if{latex}{\out{\hypertarget{method-Record-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Record$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
