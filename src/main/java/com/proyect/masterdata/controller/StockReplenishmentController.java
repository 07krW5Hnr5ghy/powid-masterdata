package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.request.RequestStockReplenishmentItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IStockReplenishment;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("stock-replenishment")
@AllArgsConstructor
public class StockReplenishmentController {
    private final IStockReplenishment iStockReplenishment;
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    private ResponseEntity<ResponseSuccess> save(
            @RequestParam("orderId") Long orderId,
            @RequestBody() List<RequestStockReplenishmentItem> requestStockReplenishmentItems,
            @RequestParam("tokenUser") String tokenUser
            ) throws BadRequestExceptions {
        ResponseSuccess result = iStockReplenishment.save(orderId,requestStockReplenishmentItems,tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
}
