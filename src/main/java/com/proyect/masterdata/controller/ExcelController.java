package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.request.RequestPurchaseExcel;
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
    @PostMapping()
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("tokenUser") String tokenUser,
            @RequestPart("requestPurchaseExcel") RequestPurchaseExcel requestPurchaseExcel,
            @RequestPart("multipartFile") MultipartFile multipartFile
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iExcel.purchase(requestPurchaseExcel,multipartFile,tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
}
