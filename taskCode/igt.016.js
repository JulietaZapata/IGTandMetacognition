/* IOWA Gambling Task Version 0.16 
BY:  Ben Margevicius, ben@margevici.us
Usage: if you add ?email_results_to=you@somedomain.com in the query string
	   if you add ?mail_subject=subject for the email like study id it might be useful here.
EX: http://margevici.us/projects/igt/index.html?email_results_to=test@yahoo.com&mail_subject=A1234B4567
You don't have to do both.
http://margevici.us/projects/igt/index.html?email_results_to=test@yahoo.com

You can bookmark http://margevici.us/projects/igt/index.html?email_results_to=you@somewhere.com and create a shortcut for your subjects.

Please donate to the beer fund paypal: bdm4@po.cwru.edu


The MIT License (MIT)

Copyright (c) 2015 Ben Margevicius, margevici.us

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/


var totalcash = 2000, //cash in the cash pile
        deckAclicks = 0, //clicks for deck A
        deckBclicks = 0, //clicks for deck B
        deckCclicks = 0, //clicks for deck C
        deckDclicks = 0, //clicks for deck D
        totalclicks = 0, //total clicks. if == to MAXGAMES stop playing.  
        penalty = 0,   //penalty store for display
        netgain = 0,   //netgain store fpr display
        email_address = '', //where to email the data to?
        mail_attachment = '', //the results of the test that gets emailed.
        mail_subject = 'IGT data',
		mailsvc_url = '/MailSvc/MailSvc.asmx/SendMail' //Email Service. CORS is disabled so I hope this isn't exploitable.
        GAME_VERSION = "0.16",
        GAME_VERSION_DATE = new Date("February 23, 2015 01:44:00"),    
        DECKA_WIN = 100, //how much did we win on Deck A click
        DECKB_WIN = 100, //how much did we win on Deck B click
        DECKC_WIN = 50, //how much did we win on Deck C click
        DECKD_WIN = 50, //how much did we win on Deck D click
	    CASHMAX = 6000, //Maximum amount of cash that can be won.	
	    MAXGAMES = 100; //maxium amount of plays 100

//Penaly schedules. If lookup DECKN_PENALTY[deckNclicks] to get the preset penalty amount. 
var DECKA_PENALTY = [0, 0, -150, 0, -300, 0, -200, 0, -250, -350, 0, -350, 0, -250, -200, 0, -300, -150, 0, 0, 0, -300, 0, -350, 0, -200, -250, -150, 0, 0, -350, -200, -250, 0, 0, 0, -150, -300, 0, 0];
var DECKB_PENALTY = [0, 0, 0, 0, 0, 0, 0, 0, -1250, 0, 0, 0, 0, -1250, 0, 0, 0, 0, 0, 0, -1250, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1250, 0, 0, 0, 0, 0, 0, 0, 0];
var DECKC_PENALTY = [0, 0, -50, 0, -50, 0, -50, 0, -50, -50, 0, -25, -75, 0, 0, 0, -25, -75, 0, -50, 0, 0, 0, -50, -25, -50, 0, 0, -75, -50, 0, 0, 0, -25, -25, 0, -75, 0, -50, -75];
var DECKD_PENALTY = [0, 0, 0, 0, 0, 0, 0, 0, 0, -250, 0, 0, 0, 0, 0, 0, 0, 0, 0, -250, 0, 0, 0, 0, 0, 0, 0, 0, -250, 0, 0, 0, 0, 0, -250, 0, 0, 0, 0, 0];
var selectedCards = []; // stores the selections for output when the game is over.
var trialnumber   = []; // stores trial number
var totalcash_by_trial = []; // stores the amount of cash after each trial
var win  = []; //stores how much money the subject received for her choice
var loss = []; //stores how much money the subject lost for her choice
var net_gain      = []; // stores how much $ did de subject win/lose
var confidence    = []; // stores confidence report on each trial
var trial = 0; // counter
var rt_cardselection = []; // stores response times for card choices
var rt_confidence = []; // stores response times for confidence report
// to calculate rt:
var start1 = [];
var newtrial_time = [];
var startconf = []

//rewards preprogramed pentalties are higher for deck A & B.
$(function () {
    $('#boton_comenzar').click(function(){
        start1 = +new Date();  // to calculate response times   
    });
    
    $(".card").click(function () {
        totalclicks++; //increment our click counter.
        //Note in order to end the game the person has to click MAXGAMES + 1 times. This is ok becuase the person is just clicking away.
        
        // response times
        if(trial == 0){
            var rt = +new Date() - start1;
            rt_cardselection.push(rt);
        } else{
            var rt = +new Date() - newtrial_time;
            rt_cardselection.push(rt);
        }
        startconf = +new Date();

        trial = trial + 1;  // counting trials...
        trialnumber.push(trial); // save trialnumber
        
        if (trial <= MAXGAMES) {

            var clicked = $(this).attr("id"); //Get the id of the clicked deck
            switch (clicked) {                //Do something with that clicked deck id.
                case "card-one":
                    if (deckAclicks === DECKA_PENALTY.length)
                    {
                        //if we are at the end of the array reset our position back to the beginning. this is described in variants of this test.
                        deckAclicks = 0;
                    }   
                    penalty = DECKA_PENALTY[deckAclicks]; //get the penalty value
                    netgain = DECKA_WIN + penalty;          //get the net gain                    
                    $("#winamt").html(DECKA_WIN);           //output our win amount                   
                    deckAclicks++;                        //increment our position for penalty lookup
                    selectedCards.push("A");                //Add to our output of selected cards.
                    net_gain.push(netgain);
                    win.push(100);
                    loss.push(penalty);
                    //$("#deck-one-clicks").html(deckoneclicks); debugging                    
                    break;

                case "card-two":
                    if (deckBclicks === DECKB_PENALTY.length) {
                        //if we are at the end of the array reset our position back to the beginning. this is described in variants of this test.
                        deckBclicks = 0;
                    }
                    penalty = DECKB_PENALTY[deckBclicks]; //get the penalty value
                    netgain = DECKB_WIN + penalty;          //get the net gain                    
                    $("#winamt").html(DECKB_WIN);           //output our win amount                   
                    deckBclicks++;                        //increment our position for penalty lookup
                    selectedCards.push("B");                //Add to our output of selected cards.
                    net_gain.push(netgain);
                    win.push(100);                    
                    loss.push(penalty);
                    //$("#deck-one-clicks").html(deckoneclicks); debugging          
                    break;

                case "card-three":
                    if (deckCclicks === DECKC_PENALTY.length) {
                        //if we are at the end of the array reset our position back to the beginning. this is described in variants of this test.
                        deckCclicks = 0;
                    }
                    penalty = DECKC_PENALTY[deckCclicks]; //get the penalty value
                    netgain = DECKC_WIN + penalty;          //get the net gain                    
                    $("#winamt").html(DECKC_WIN);           //output our win amount                   
                    deckCclicks++;                        //increment our position for penalty lookup
                    selectedCards.push("C");                //Add to our output of selected cards.
                    net_gain.push(netgain);
                    win.push(50);          
                    loss.push(penalty);          
                    //$("#deck-one-clicks").html(deckoneclicks); debugging                    
                    break;

                case "card-four":
                    if (deckDclicks === DECKD_PENALTY.length) {
                        //if we are at the end of the array reset our position back to the beginning. this is described in variants of this test.
                        deckDclicks = 0;
                    }
                    penalty = DECKD_PENALTY[deckDclicks]; //get the penalty value
                    netgain = DECKD_WIN + penalty;          //get the net gain                    
                    $("#winamt").html(DECKD_WIN);           //output our win amount                   
                    deckDclicks++;                        //increment our position for penalty lookup
                    selectedCards.push("D");                //Add to our output of selected cards.
                    net_gain.push(netgain);
                    win.push(50);
                    loss.push(penalty);
                    //$("#deck-one-clicks").html(deckoneclicks); debugging                    
                    break;
            }

            $('#cashpilelabel').hide();
            $('.progress').hide();
            $('#gamelog').hide();
            $('.confbuttons').css('visibility', 'visible');
            $('.card').hide();
            $('#confquestion').css('visibility', 'visible');
            
            
            $("#conf1").click(function(){
                confidence.push(1);
                $('#conf1').unbind('click');
            });
    
            $("#conf2").click(function(){
                confidence.push(2);
                $('#conf2').unbind('click');
            });
    
            $("#conf3").click(function(){
                confidence.push(3);
                $('#conf3').unbind('click');
            });
    
            $("#conf4").click(function(){
                confidence.push(4);
                $('#conf4').unbind('click');
            });
            $('.confbuttons').click(function(){
                // confidence button clicked, store response time
                var rt2 = +new Date() - startconf;
                rt_confidence.push(rt2);

                $('#clickparacontinuar').css('visibility','visible');
                $('#cashpilelabel').show();
                $('.progress').show();
                $('#gamelog').show();
                $('.confbuttons').css('visibility', 'hidden');
                $('#confquestion').css('visibility', 'hidden');
                

                $("#penaltyamt").html(penalty.toString());  //output the penalty
                
                $("#netgains").html(netgain.toString());    //output the net gain or loss
                totalcash += netgain;                       //increment our totals
                //change the color of the font if we win or lose
                if (netgain <= 0)
                    $(".outputtext").css("color", "red");
                else
                    $(".outputtext").css("color", "blue");
    
                if (totalcash < 0) totalcash = 0; //if total cash is negative make it 0.
                totalcash_by_trial.push(totalcash);
    
                $("#totalmoney").html("$" + totalcash.toString());
                //calculate our cash bar and display
                var cashpilebarvalue = 100 * totalcash / CASHMAX;
                $("#cashpilebar").css("width", cashpilebarvalue.toString() + "%"); //grow or shrink the progress bar
                $("#cashpileamt").html("$" + totalcash);                            //change the label in the progress bar
                $('.confbuttons').unbind('click');
                
            });
            
            $('#clickparacontinuar').click(function(){
                newtrial_time = +new Date();
                $('.card').show();
                $('#clickparacontinuar').css('visibility', 'hidden');
                $('#clickparacontinuar').unbind('click');
            })


        }
        else //game over 
        {   
            $(".card").hide();
            $(".row").hide();
            $("#finishbutton1").css('visibility', 'visible');
            $("#finishbutton2").css('visibility', 'visible');
            $('.final').css('visibility', 'visible');

            var name = document.getElementById('Name').value;
            var gender = document.getElementById('Gender').value;
            var age = document.getElementById('Age').value;
            var email = document.getElementById('email').value;

            var datos = {
                'name': name,
                'gender': gender,
                'age': age,
                'email': email,
                "trial_number": trialnumber,
                "card_selected": selectedCards,
                "rt_cardselection": rt_cardselection,
                "totalcash_by_trial": totalcash_by_trial,
                "win": win,
                "loss": loss,
                "net_gain": net_gain,
                "confidence": confidence,
                "rt_confidence": rt_confidence,
            };
            
            jatos.appendResultData(JSON.stringify(datos));
            
            $("#finishbutton1").click(function (){
                jatos.endStudyAndRedirect("https://google.com.ar/");
            });
            $("#finishbutton2").click(function (){
                jatos.endStudyAndRedirect("https://sites.google.com/view/experimentosonline");
            });
            
        }
    });
});