package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.request.RequestStockReturn;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IStockReturn;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("stock-return")
@AllArgsConstructor
public class StockReturnController {
    private final IStockReturn iStockReturn;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> save(
            @RequestBody() List<RequestStockReturn> requestStockReturnList,
            @RequestParam("purchaseSerial") String purchaseSerial,
            @RequestParam("tokenUser") String tokenUser
            ) throws BadRequestExceptions {
        ResponseSuccess result = iStockReturn.save(requestStockReturnList,purchaseSerial,tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
}
