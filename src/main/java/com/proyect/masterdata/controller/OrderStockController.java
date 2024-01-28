package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.request.RequestOrderStock;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.services.IOrderStock;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({"*"})
@RequestMapping("order-stock")
@AllArgsConstructor
public class OrderStockController {

    private final IOrderStock iOrderStock;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("orderId") Long orderId,
            @RequestBody() List<RequestOrderStock> requestOrderStockList,
            @RequestParam("tokenUser") String tokenUser
            ) throws InternalErrorExceptions, BadRequestExceptions{
        ResponseSuccess result = iOrderStock.save(orderId,requestOrderStockList,tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
}
