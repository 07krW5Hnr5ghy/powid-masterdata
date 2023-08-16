package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.SizeTypeDTO;
import com.proyect.masterdata.dto.request.RequestSizeType;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.ISizeType;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({"*"})
@RequestMapping("/size-type")
@AllArgsConstructor
public class SizeTypeController {
    private final ISizeType iSizeType;
    @PostMapping()
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name,@RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseSuccess result = iSizeType.save(name,user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "/size-types")
    public ResponseEntity<ResponseSuccess> saveall(
            @RequestBody() List<String> names,@RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseSuccess result = iSizeType.saveAll(names,user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PutMapping()
    public ResponseEntity<SizeTypeDTO> update(
            @RequestBody() RequestSizeType requestSizeType
    ) throws BadRequestExceptions {
        SizeTypeDTO result = iSizeType.update(requestSizeType);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping()
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("code") Long code,
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseDelete result = iSizeType.delete(code,user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping(value = "/size-types")
    public ResponseEntity<ResponseDelete> deleteall(
            @RequestBody() List<Long> codes,
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseDelete result = iSizeType.deleteAll(codes,user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping()
    public ResponseEntity<List<SizeTypeDTO>> list() throws BadRequestExceptions {
        List<SizeTypeDTO> result = iSizeType.list();
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value="/statusFalse")
    public ResponseEntity<List<SizeTypeDTO>> listStatusFalse() throws BadRequestExceptions {
        List<SizeTypeDTO> result = iSizeType.listStatusFalse();
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/code")
    public ResponseEntity<SizeTypeDTO> findByCode(
            @RequestParam("code") Long code
    ) throws BadRequestExceptions {
        SizeTypeDTO result = iSizeType.findByCode(code);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/name")
    public ResponseEntity<SizeTypeDTO> findByName(
            @RequestParam("name") String name
    ) throws BadRequestExceptions {
        SizeTypeDTO result = iSizeType.findByName(name);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/user")
    public ResponseEntity<List<SizeTypeDTO>> findByUser(
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        List<SizeTypeDTO> result = iSizeType.findByUser(user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
}
