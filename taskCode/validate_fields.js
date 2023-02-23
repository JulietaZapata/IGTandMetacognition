function validateFields()
{	var Name 		= document.getElementById("Name").value;
	var Gender 		= document.getElementById("Gender").value;
	var Age 		= document.getElementById("Age").value;
	var Email       = document.getElementById('email').value;
	
	var Emailtest = false;
	if(Email.trim() != ""){
		const re = /^(([^<>()\[\]\\.,;:\s@"]+(\.[^<>()\[\]\\.,;:\s@"]+)*)|(".+"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/;
		Emailtest =  re.test(String(Email).toLowerCase());
	}

	if ( ((Age>=18) && (Age<99)) && ((Gender == "f") || (Gender == "m") || (Gender == "noBinario") || (Gender == "Pref_no_responder")) && (Emailtest))
	{
		$("#boton_comenzar").fadeTo(.1, 1);
		$("#boton_comenzar").prop('disabled', false);
	}
	else
	{
		$("#boton_comenzar").fadeTo(.1, .5);
		$("#boton_comenzar").prop('disabled', true);
	}
}

