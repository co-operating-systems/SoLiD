package run.cosy.http.util

import akka.http.scaladsl.model.Uri
import Uri._

extension (uri: Uri)

	/** return filename if exists */
	def fileName: Option[String] = uri.path.reverse match
		case Path.Segment(head,tail) => Some(head)
		case _ => None
		
	/** remove uri without the final slash, or the same */
	def withoutSlash: Uri =
		val rev: Uri.Path = uri.path.reverse
		rev match
			case Uri.Path.Slash(path) => uri.withPath(path.reverse)
			case _ => uri

	/**
	 * replace fileName with Name in Uri or else place filename after slash or add an initial slash
	 * Todo: improve - this definintion feels very ad-hoc ...
	 **/
	def sibling(name: String) =
		val rev: Uri.Path = uri.path.reverse
		val newPath = rev match
			case Path.Slash(path) => uri.path ++ Path(name)
			case Path.SingleSlash => Path.Slash(Path(name))
			case Path.Empty => Path.Slash(Path(name))
			case Path.Segment(head, tail) => Path.Segment(name,tail).reverse
		uri.withPath(newPath)
