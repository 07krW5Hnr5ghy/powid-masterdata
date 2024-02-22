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
            @RequestParam(value = "type",required = false) String paymentType
    ) throws MPException, MPApiException {
        if(dataId != null & Objects.equals(paymentType, "payment")){
            PaymentClient paymentClient = new PaymentClient();
            Payment newPayment = paymentClient.get(dataId);
            System.out.println(newPayment);
            return "Payment";
        }else {
            return "No payment";
        }
    }

}
