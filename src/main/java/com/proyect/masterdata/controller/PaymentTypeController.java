package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.PaymentTypeDTO;
import com.proyect.masterdata.dto.request.RequestPaymentTypeSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IPaymentType;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({"*"})
@RequestMapping("/payment-type")
@AllArgsConstructor
public class PaymentTypeController {
    private final IPaymentType iPaymentType;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("type") String type,
            @RequestParam("user") String user
            ) throws BadRequestExceptions {
        ResponseSuccess result = iPaymentType.save(type,user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "/payment-types",consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> saveAll(
            @RequestBody() List<String> names,
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseSuccess result = iPaymentType.saveAll(names,user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("type") String type,
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseDelete result = iPaymentType.delete(type,user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Page<PaymentTypeDTO>> list(
            @RequestParam(value = "name",required = false) String name,
            @RequestParam(value = "user",required = false) String user,
            @RequestParam(value = "sort",required = false) String sort,
            @RequestParam(value = "sortColumn",required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize
    ) throws BadRequestExceptions{
        Page<PaymentTypeDTO> result = iPaymentType.list(name,user,sort,sortColumn,pageNumber,pageSize);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

    @GetMapping(value="/statusFalse",consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Page<PaymentTypeDTO>> listStatusFalse(
            @RequestParam(value = "name",required = false) String name,
            @RequestParam(value = "user",required = false) String user,
            @RequestParam(value = "sort",required = false) String sort,
            @RequestParam(value = "sortColumn",required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize
    ) throws BadRequestExceptions {
        Page<PaymentTypeDTO> result = iPaymentType.listStatusFalse(name,user,sort,sortColumn,pageNumber,pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
}
