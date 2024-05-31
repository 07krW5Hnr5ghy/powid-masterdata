package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.request.RequestPurchaseExcel;
import com.proyect.masterdata.dto.request.RequestShipmentExcel;
import com.proyect.masterdata.dto.request.RequestStockReturnExcel;
import com.proyect.masterdata.dto.request.RequestStockTransferExcel;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IExcel;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("excel")
@AllArgsConstructor
public class ExcelController {
    private final IExcel iExcel;
    @PostMapping("purchase")
    public ResponseEntity<ResponseSuccess> purchase(
            @RequestPart("requestPurchaseExcel") RequestPurchaseExcel requestPurchaseExcel,
            @RequestPart("multipartFile") MultipartFile multipartFile
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iExcel.purchase(requestPurchaseExcel,multipartFile);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @PostMapping("shipment")
    public ResponseEntity<ResponseSuccess> shipment(
            @RequestPart("requestShipmentExcel") RequestShipmentExcel requestShipmentExcel,
            @RequestPart("multipartFile") MultipartFile multipartFile
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iExcel.shipment(requestShipmentExcel,multipartFile);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @PostMapping("transfer")
    public ResponseEntity<ResponseSuccess> transfer(
            @RequestPart("requestStockTransferExcel") RequestStockTransferExcel requestStockTransferExcel,
            @RequestPart("multipartFile") MultipartFile multipartFile
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iExcel.stockTransfer(requestStockTransferExcel,multipartFile);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @PostMapping("return")
    public ResponseEntity<ResponseSuccess> stockReturn(
            @RequestPart("requestStockReturnExcel") RequestStockReturnExcel requestStockReturnExcel,
            @RequestPart("multipartFile") MultipartFile multipartFile
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iExcel.stockReturn(requestStockReturnExcel,multipartFile);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @PostMapping("replenishment")
    public ResponseEntity<ResponseSuccess> stockReplenishment(
            @RequestParam("orderId") Long orderId,
            @RequestPart("multipartFile") MultipartFile multipartFile,
            @RequestParam("tokenUser") String tokenUser
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iExcel.stockReplenishment(orderId,multipartFile,tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @PostMapping("order-stock")
    public ResponseEntity<ResponseSuccess> orderStock(
            @RequestParam("orderId") Long orderId,
            @RequestParam("warehouse") String warehouse,
            @RequestPart("multipartFile") MultipartFile multipartFile,
            @RequestParam("tokenUser") String tokenUser
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iExcel.orderStock(orderId,warehouse,multipartFile,tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @PostMapping("order-return")
    public ResponseEntity<ResponseSuccess> orderReturn(
            @RequestParam("orderId") Long orderId,
            @RequestPart("multipartFile") MultipartFile multipartFile,
            @RequestParam("tokenUser") String tokenUser
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iExcel.orderReturn(orderId,multipartFile,tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @PostMapping("product")
    public ResponseEntity<ResponseSuccess> product(
            @RequestPart("multipartFile") MultipartFile multipartFile,
            @RequestParam("tokenUser") String tokenUser
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iExcel.product(multipartFile,tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
}
