
function(doc) {
	if (doc.score > 0)
        emit(doc.score)
}