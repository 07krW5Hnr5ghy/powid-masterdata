package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.request.RequestOrderItem;
import com.proyect.masterdata.dto.response.ResponseCheckStockItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IOrderItem;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@CrossOrigin({"*"})
@RequestMapping("order-item")
@AllArgsConstructor
public class OrderItemController {

    private final IOrderItem iOrderItem;

    @GetMapping()
    public ResponseEntity<ResponseCheckStockItem> checkStockItem(
            @RequestParam("productSku") String productSku,
            @RequestParam("quantity") Integer quantity,
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseCheckStockItem result = iOrderItem.checkStock(productSku, quantity, user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> addItem(
            @RequestParam("orderId") Long orderId,
            @RequestBody()RequestOrderItem requestOrderItem,
            @RequestParam("tokenUser") String tokenUser
            ) throws BadRequestExceptions {
        ResponseSuccess result = iOrderItem.add(orderId,requestOrderItem,tokenUser);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }
}
