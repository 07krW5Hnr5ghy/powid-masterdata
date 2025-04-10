package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.OrderDTO;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IPaymentMetodClient;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({"*"})
@RequestMapping("payment-metod-client")
@AllArgsConstructor
public class PaymentMetodClientController {
    private final IPaymentMetodClient paymentMetodClient;

    @GetMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:SALES','ROLE:CUSTOMER_SERVICE','ROLE:STOCK') and hasAuthority('ACCESS:ORDER_GET')")
    public ResponseEntity<List<OrderDTO>> listOrders(
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<OrderDTO>> result = null;
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }


}
