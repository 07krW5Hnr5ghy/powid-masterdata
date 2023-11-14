package com.proyect.masterdata.controller;

import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;

import com.proyect.masterdata.dto.request.RequestProductSave;
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
            @RequestBody() RequestProductSave product,
            @RequestParam("user") String user) throws BadRequestExceptions {
        ResponseSuccess result = iProduct.save(product, user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
}
