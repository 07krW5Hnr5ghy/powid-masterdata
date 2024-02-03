package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.response.ResponseCheckStockItem;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IItem;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@CrossOrigin({"*"})
@RequestMapping("item")
@AllArgsConstructor
public class ItemController {

    private final IItem iItem;

    @GetMapping()
    public ResponseEntity<ResponseCheckStockItem> checkStockItem(
            @RequestParam("productSku") String productSku,
            @RequestParam("quantity") Integer quantity,
            @RequestParam("tokenUser") String tokenUser
    ) throws BadRequestExceptions {
        ResponseCheckStockItem result = iItem.checkStock(productSku, quantity, tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
}
