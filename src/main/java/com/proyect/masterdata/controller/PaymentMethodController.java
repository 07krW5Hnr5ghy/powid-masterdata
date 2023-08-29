package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.PaymentMethodDTO;
import com.proyect.masterdata.dto.request.RequestPaymentMethod;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IPaymentMethod;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({"*"})
@RequestMapping("/payment-method")
@AllArgsConstructor
public class PaymentMethodController {


    private final IPaymentMethod iPaymentMethod;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name,@RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseSuccess result = iPaymentMethod.save(name,user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "/payment-methods",consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> saveall(
            @RequestBody() List<String> names,@RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseSuccess result = iPaymentMethod.saveAll(names,user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PutMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<PaymentMethodDTO> update(
            @RequestBody() RequestPaymentMethod requestPaymentMethod
    ) throws BadRequestExceptions {
        PaymentMethodDTO result = iPaymentMethod.update(requestPaymentMethod);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("code") Long code,
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseDelete result = iPaymentMethod.delete(code,user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/list-payment-method",consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<List<PaymentMethodDTO>> listPaymentMethod() throws BadRequestExceptions {
        List<PaymentMethodDTO> result = iPaymentMethod.listPaymentMethod();
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Page<PaymentMethodDTO>> list(
            @RequestParam(value = "name",required = false) String name,
            @RequestParam(value = "user",required = false) String user,
            @RequestParam(value = "sort",required = false) String sort,
            @RequestParam(value = "sortColumn",required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize
    ) throws BadRequestExceptions {
        Page<PaymentMethodDTO> result = iPaymentMethod.list(name,user,sort,sortColumn,pageNumber,pageSize);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

    @GetMapping(value="/statusFalse",consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Page<PaymentMethodDTO>> listStatusFalse(
            @RequestParam(value = "name",required = false) String name,
            @RequestParam(value = "user",required = false) String user,
            @RequestParam(value = "sort",required = false) String sort,
            @RequestParam(value = "sortColumn",required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize
    ) throws BadRequestExceptions {
        Page<PaymentMethodDTO> result = iPaymentMethod.listStatusFalse(name,user,sort,sortColumn,pageNumber,pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/code",consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<PaymentMethodDTO> findByCode(
            @RequestParam("code") Long code
    ) throws BadRequestExceptions {
        PaymentMethodDTO result = iPaymentMethod.findByCode(code);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

}
