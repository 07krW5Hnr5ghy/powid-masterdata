package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.request.RequestSubscriptionPayment;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.ISubscriptionPayment;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("subscription-payment")
@AllArgsConstructor
public class SubscriptionPaymentController {

    private final ISubscriptionPayment iSubscriptionPayment;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    @PreAuthorize("hasAuthority('ROLE:BUSINESS') and hasAuthority('ACCESS:SUBSCRIPTION_PAYMENT_POST')")
    public ResponseEntity<String> sendPayment(
            @RequestBody()RequestSubscriptionPayment requestSubscriptionPayment,
            @RequestParam("tokenUser") String tokenUser
            ) throws BadRequestExceptions {
        String result = iSubscriptionPayment.send(requestSubscriptionPayment,tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "demo")
    @PreAuthorize("hasAuthority('ROLE:BUSINESS') and hasAuthority('ACCESS:SUBSCRIPTION_PAYMENT_POST')")
    public ResponseEntity<ResponseSuccess> activeDemo(
            @RequestParam("username") String username
    ) throws BadRequestExceptions {
        ResponseSuccess result = iSubscriptionPayment.activateDemo(username);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }
}
