package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.MasterListDTO;
import com.proyect.masterdata.dto.SaleChannelDTO;
import com.proyect.masterdata.dto.SizeDTO;
import com.proyect.masterdata.dto.request.RequestMasterList;
import com.proyect.masterdata.dto.request.RequestSaleChannel;
import com.proyect.masterdata.dto.request.RequestSize;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseMasterList;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.handler.ErrorResponse;
import com.proyect.masterdata.services.IMasterList;
import com.proyect.masterdata.services.ISize;
import com.proyect.masterdata.services.impl.SizeImpl;
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
@RequestMapping("/size")
@AllArgsConstructor
public class SizeController {

    private final ISize iSize;
    @PostMapping()
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name
    ) throws BadRequestExceptions {
        ResponseSuccess result = iSize.save(name);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "/departments")
    public ResponseEntity<ResponseSuccess> saveall(
            @RequestBody() List<String> names
    ) throws BadRequestExceptions {
        ResponseSuccess result = iSize.saveAll(names);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PutMapping()
    public ResponseEntity<SizeDTO> update(
            @RequestBody() RequestSize requestSize
    ) throws BadRequestExceptions {
        SizeDTO result = iSize.update(requestSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping()
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("code") Long code
    ) throws BadRequestExceptions {
        ResponseDelete result = iSize.delete(code);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping(value = "/departments")
    public ResponseEntity<ResponseDelete> deleteall(
            @RequestBody() List<Long> codes
    ) throws BadRequestExceptions {
        ResponseDelete result = iSize.deleteAll(codes);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping()
    public ResponseEntity<List<SizeDTO>> list() throws BadRequestExceptions {
        List<SizeDTO> result = iSize.list();
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/code")
    public ResponseEntity<SizeDTO> findByCode(
            @RequestParam("code") Long code
    ) throws BadRequestExceptions {
        SizeDTO result = iSize.findByCode(code);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/name")
    public ResponseEntity<SizeDTO> findByName(
            @RequestParam("name") String name
    ) throws BadRequestExceptions {
        SizeDTO result = iSize.findByName(name);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

}
