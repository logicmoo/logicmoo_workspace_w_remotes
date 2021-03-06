var TimeInference = /** @class */ (function () {
    function TimeInference() {
    }
    // This function replaces all Sentences that have a past sentence by their immediately past value:
    TimeInference.applyTimePast = function (sc) {
        var newsc = new SentenceContainer();
        console.log("TimeInference.applyTimePast!");
        for (var _i = 0, _a = sc.previousSentencesWithNoCurrentSentence; _i < _a.length; _i++) {
            var s = _a[_i];
            //			console.log("from previousSentencesWithNoCurrentSentence: " + s.sentence);
            // we have to use "addStateSentenceIfNew" here, since some of these might overwrite each other...
            newsc.addStateSentenceIfNew(s.sentence, s.provenance, s.activation, s.time);
        }
        for (var _b = 0, _c = sc.plainSentenceList; _b < _c.length; _b++) {
            var s = _c[_b];
            if (s.previousInTime == null) {
                newsc.addSentence(s.sentence, s.provenance, s.activation, s.time);
            }
            else {
                //				console.log("from plainSentenceList.previousInTime: " + s.previousInTime.sentence);
                newsc.addSentence(s.previousInTime.sentence, s.previousInTime.provenance, s.previousInTime.activation, s.previousInTime.time);
            }
        }
        return newsc;
    };
    TimeInference.timeMatch = function (current, start, end, timeTerm) {
        /*
                // TODO:
                <sort name="time.at" super="time,relation"/>
                <sort name="time.subsequently" super="time.later,relation"/>
                <sort name="time.today" super="time.now"/>
                <sort name="time.yesterday" super="time.past"/>
                <sort name="time.date" super="time"/>
        */
        if (timeTerm.functor.name == "time.past") {
            if (start < current)
                return true;
            return false;
        }
        else if (timeTerm.functor.name == "time.present" ||
            timeTerm.functor.name == "time.now") {
            if (start <= current &&
                (end == null || end > current))
                return true;
        }
        else if (timeTerm.functor.name == "time.future" ||
            timeTerm.functor.name == "time.later") {
            if (end == null || end > current)
                return true;
        }
        else if (timeTerm.functor.name == "time.today") {
            var dayStart = Math.floor(current / (60 * 60 * 24)) * 60 * 60 * 24;
            var dayEnd = dayStart + 60 * 60 * 24;
            if (start < dayEnd &&
                (end == null || end >= dayStart))
                return true;
        }
        else if (timeTerm.functor.name == "time.yesterday") {
            var dayStart = (Math.floor(current / (60 * 60 * 24)) - 1) * 60 * 60 * 24;
            var dayEnd = dayStart + 60 * 60 * 24;
            if (start < dayEnd &&
                (end == null || end >= dayStart))
                return true;
        }
        return false;
    };
    return TimeInference;
}());
