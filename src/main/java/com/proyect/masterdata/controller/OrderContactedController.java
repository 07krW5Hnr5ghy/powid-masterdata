package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IOrderContacted;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("order-contacted")
@AllArgsConstructor
public class OrderContactedController {
    private final IOrderContacted iOrderContacted;
    @PutMapping()
    public ResponseEntity<ResponseSuccess> activate(
            @RequestParam("orderId") UUID orderId,
            @RequestParam("tokenUser") String tokenUser
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iOrderContacted.markContacted(orderId,tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
}
