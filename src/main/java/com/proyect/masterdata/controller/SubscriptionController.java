package com.proyect.masterdata.controller;

import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;

import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.services.ISubscription;

import lombok.AllArgsConstructor;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("subscription")
@AllArgsConstructor
public class SubscriptionController {

    private final ISubscription iSubscription;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name,
            @RequestParam("months") Integer months,
            @RequestParam("discountPercent") Double discountPercent,
            @RequestParam("tokenUser") String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        ResponseSuccess result = iSubscription.save(name, months, discountPercent, tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
}
