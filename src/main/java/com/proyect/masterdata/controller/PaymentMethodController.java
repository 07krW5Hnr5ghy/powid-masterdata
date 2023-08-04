package com.proyect.masterdata.controller;

import com.proyect.masterdata.domain.PaymentMethod;
import com.proyect.masterdata.dto.MasterListDTO;
import com.proyect.masterdata.dto.MembershipTypeDTO;
import com.proyect.masterdata.dto.PaymentMethodDTO;
import com.proyect.masterdata.dto.request.RequestMasterList;
import com.proyect.masterdata.dto.request.RequestMembershipType;
import com.proyect.masterdata.dto.request.RequestPaymentMethod;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseMasterList;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.handler.ErrorResponse;
import com.proyect.masterdata.services.IMasterList;
import com.proyect.masterdata.services.IPaymentMethod;
import com.proyect.masterdata.services.impl.PaymentMethodImpl;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({"*"})
@RequestMapping("/payment-method")
@AllArgsConstructor
public class PaymentMethodController {


    private final IPaymentMethod iPaymentMethod;

    @PostMapping()
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name
    ) throws BadRequestExceptions {
        ResponseSuccess result = iPaymentMethod.save(name);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "/payment-methods")
    public ResponseEntity<ResponseSuccess> saveall(
            @RequestBody() List<String> names
    ) throws BadRequestExceptions {
        ResponseSuccess result = iPaymentMethod.saveAll(names);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PutMapping()
    public ResponseEntity<PaymentMethodDTO> update(
            @RequestBody() RequestPaymentMethod requestPaymentMethod
    ) throws BadRequestExceptions {
        PaymentMethodDTO result = iPaymentMethod.update(requestPaymentMethod);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping()
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("code") Long code
    ) throws BadRequestExceptions {
        ResponseDelete result = iPaymentMethod.delete(code);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping(value = "/payment-methods")
    public ResponseEntity<ResponseDelete> deleteall(
            @RequestBody() List<Long> codes
    ) throws BadRequestExceptions {
        ResponseDelete result = iPaymentMethod.deleteAll(codes);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping()
    public ResponseEntity<List<PaymentMethodDTO>> list() throws BadRequestExceptions {
        List<PaymentMethodDTO> result = iPaymentMethod.list();
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/code")
    public ResponseEntity<PaymentMethodDTO> findByCode(
            @RequestParam("code") Long code
    ) throws BadRequestExceptions {
        PaymentMethodDTO result = iPaymentMethod.findByCode(code);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/name")
    public ResponseEntity<PaymentMethodDTO> findByName(
            @RequestParam("name") String name
    ) throws BadRequestExceptions {
        PaymentMethodDTO result = iPaymentMethod.findByName(name);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
}
