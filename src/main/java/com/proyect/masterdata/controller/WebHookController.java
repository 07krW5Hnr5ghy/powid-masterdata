package com.proyect.masterdata.controller;

import com.mercadopago.client.payment.PaymentClient;
import com.mercadopago.exceptions.MPApiException;
import com.mercadopago.exceptions.MPException;
import com.mercadopago.resources.payment.Payment;
import lombok.AllArgsConstructor;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.Objects;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("webhook")
@AllArgsConstructor
public class WebHookController {

    @PostMapping()
    public String receive(
            @RequestParam(value = "id",required = false) String id,
            @RequestParam(value = "topic",required = false) String topic,
            @RequestParam(value = "data.id",required = false) Long dataId,
            @RequestParam(value = "type",required = false) String type
    ) throws MPException, MPApiException {
        /* in production must extract header x-signature-id with secret code from the notification and
        must coincide with the secret code in the mercado pago account of the seller for web hook notifications
         */
        /*
        answer web hook notification with 200 status to avoid of mercado pago of resending the web hook
        * */
        if(dataId != null & Objects.equals(type, "payment")){
            PaymentClient paymentClient = new PaymentClient();
            Payment newPayment = paymentClient.get(dataId);
            System.out.println(newPayment.getId());
            return "Payment";
        }else {
            return "No payment";
        }
    }

}
