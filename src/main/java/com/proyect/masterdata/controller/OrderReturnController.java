package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.OrderReturnDTO;
import com.proyect.masterdata.dto.request.RequestOrderReturnItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IOrderReturn;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("order-return")
@AllArgsConstructor
public class OrderReturnController {
    private final IOrderReturn iOrderReturn;
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("orderId") Long orderId,
            @RequestBody() List<RequestOrderReturnItem> requestOrderReturnItemList,
            @RequestParam("tokenUser") String tokenUser
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iOrderReturn.save(orderId,requestOrderReturnItemList,tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @GetMapping()
    public ResponseEntity<List<OrderReturnDTO>> list(
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<OrderReturnDTO>> result = iOrderReturn.list(user);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
}
