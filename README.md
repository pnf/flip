flip
====

This is dual-purpose experiment.

* Make a better pdf reader.

  One reason thing I don't like about ebook readers is that you can't
  flip through the pages properly.  You can search for things.  You
  can use internal hyperlinks.  But, with a real book, you often flip
  quickly through the pages, usually recognizing what you were looking
  for slightly after it's past, and then backtracking more slowly.

  Since physical pages can flip by at hundreds of pages per second, it
  isn't feasible to display successive pdf pages quickly enough to
  duplicate the physical experience.  However, you can precalculate a
  rolling average of pages and, when the scroll widget is moved
  quickly, display them blurred together -
  effectively duplicating your eye's latent image retention.

  It works.  The current implementation hasn't been much optimized, so
  it's both slow and leaves something to be desired in terms of
  ergonomics.  For example, the averaging window is always 8 pages,
  and the view is always centered in it.

* Use functional programming concepts to solve a real problem in
  concurrent application development.

  Specifically, the computation of blurred pages takes some time, and
  since it's a rolling average, it needs to proceed sequentially.  At
  the same time, I would like the application to come up immediately
  and to behave _as well as it can_, i.e. if the blurred image hasn't
  yet been calculated for this region, display an unprocessed image,
  while a progress counter ticks up.

  A boring,procedural way to handle this is to start a thread that
  processes the pages in a big `for` loop and have some sort of progress
  variable, e.g. an `AtomicInteger` so we know what to show. But
  that's, well, boring, and also potentially abusable.  Instead:
  1. We want to express the algorithm as a composition of list
     operations, i.e.
	 
			 val pagesView = pdfdoc.view.
			   map(imageToPixels).
			   map(unpackIntsToLongs).
			   scanLeftLazy(new Rolling(navg,npix))((r,ll)=>r.push(ll)).
			   map(r => r.avg).
			   map(packLongsToInts).
			   map(pixelsToBufferedImage).
			   zipWithIndex.
			   map( p => new Page(p._1,p._2))

     Without knowing exactly what this does, you can kind of get the
     idea immediately.

  2. We don't want the steps (lines in the above code) to run
     completely before the next one runs.  That would be bad since all
     pages would complete at the same time, and because we'd have to
     stage each intermediate result in a possibly enormous list.  The
     `view` method converts the `Iterable` to an `IterableView`, which
     has the property of calculating elements on demand.  Furthermore,
     chained list methods _compose_ their respective functions, e.g.

			iterableThing.view.map(x=>f(x)).map(x=>g(x))

     is implemented as

			iterableThing.view.map(x=>g(f(x)))

     So each page is processed completely, and separately from the others.

  3. We want to get intermediate results as `Future` objects, which
     represent a computation proceeding in another thread and can tell
     us whether they've finished or not.  That's accomplished in the
     code with the line:

	    val pages : Seq[Future[Page]] = viewToSequentialFutures(pagesView)

     (where we wrote the `viewToSequentialFutures` function) instead
     of

	    val pages : Seq[Page] = pagesView.force

     (where `force` is already a method of the various `View`
     classes).  The latter would block until all the pages are
     finished.  The former returns immediately.

  Getting this to work properly took surprisingly little code, but
  that code took me a long time to write.  I think it's a nice
  paradigm.






