package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.ICustomerType;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({"*"})
@RequestMapping("customer-type")
@AllArgsConstructor
public class CustomerTypeController {
    private final ICustomerType iCustomerType;
    @PostMapping()
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name,
            @RequestParam("tokenUser") String tokenUser
    ) throws BadRequestExceptions {
        ResponseSuccess result = iCustomerType.save(name,tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
    @GetMapping()
    public ResponseEntity<List<String>> list() throws BadRequestExceptions{
        List<String> result = iCustomerType.list();
        return new ResponseEntity<>(result,HttpStatus.OK);
    }
}
