package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.OrderReturnItemDTO;
import com.proyect.masterdata.dto.request.RequestOrderReturnItem;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.services.IOrderReturnItem;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("order-return-item")
@AllArgsConstructor
public class OrderReturnItemController {
    private final IOrderReturnItem iOrderReturnItem;
    @GetMapping()
    private ResponseEntity<List<OrderReturnItemDTO>> list(
            @RequestParam("user") String user,
            @RequestParam(value = "orderId",required = false) Long orderId
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<OrderReturnItemDTO>> result = iOrderReturnItem.list(user,orderId);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @PostMapping()
    private ResponseEntity<ResponseSuccess> save(
            @RequestParam("tokenUser") String tokenUser,
            @RequestParam("orderId") Long orderId,
            @RequestBody()RequestOrderReturnItem requestOrderReturnItem
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iOrderReturnItem.save(orderId,requestOrderReturnItem,tokenUser);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
    @DeleteMapping()
    private ResponseEntity<ResponseDelete> delete(
            @RequestParam("tokenUser") String tokenUser,
            @RequestParam("orderId") Long orderId,
            @RequestParam("supplierProduct") String supplierProductSerial
    ) throws BadRequestExceptions, InternalErrorExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseDelete> result = iOrderReturnItem.delete(orderId,supplierProductSerial,tokenUser);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
}
