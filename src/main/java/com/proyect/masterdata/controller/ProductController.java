package com.proyect.masterdata.controller;

import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;

import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IProduct;

import lombok.AllArgsConstructor;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("/product")
@AllArgsConstructor
public class ProductController {

    private final IProduct iProduct;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("sku") String sku,
            @RequestParam("model") String model,
            @RequestParam("color") String color,
            @RequestParam("category") String category,
            @RequestParam("size") String size,
            @RequestParam("user") String user) throws BadRequestExceptions {
        ResponseSuccess result = iProduct.save(sku, model, size, category, color, user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
}
