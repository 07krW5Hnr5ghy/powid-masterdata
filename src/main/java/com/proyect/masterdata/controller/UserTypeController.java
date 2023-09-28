package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.UserTypeDTO;
import com.proyect.masterdata.dto.request.RequestUserType;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IUserType;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({"*"})
@RequestMapping("/user-type")
@AllArgsConstructor
public class UserTypeController {
    private IUserType iUserType;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("userType") String userType,
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseSuccess result = iUserType.save(userType,user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "/user-types",consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> saveAll(
            @RequestBody() List<String> userTypes,
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseSuccess result = iUserType.saveAll(userTypes,user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PutMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<UserTypeDTO> update(
            @RequestBody() RequestUserType requestUserType
    ) throws BadRequestExceptions {
        UserTypeDTO result = iUserType.update(requestUserType);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping()
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("code") Long code,
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseDelete result = iUserType.delete(code,user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/list",consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<List<UserTypeDTO>> listUserType() throws BadRequestExceptions {
        List<UserTypeDTO> result = iUserType.listUserType();
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Page<UserTypeDTO>> list(
            @RequestParam(value = "userType",required = false) String userType,
            @RequestParam(value = "user",required = false) String user,
            @RequestParam(value = "sort",required = false) String sort,
            @RequestParam(value = "sortColumn",required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize
    ) throws BadRequestExceptions{
        Page<UserTypeDTO> result = iUserType.list(userType,user,sort,sortColumn,pageNumber,pageSize);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

    @GetMapping(value="/statusFalse",consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Page<UserTypeDTO>> listStatusFalse(
            @RequestParam(value = "userType",required = false) String userType,
            @RequestParam(value = "user",required = false) String user,
            @RequestParam(value = "sort",required = false) String sort,
            @RequestParam(value = "sortColumn",required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize
    ) throws BadRequestExceptions {
        Page<UserTypeDTO> result = iUserType.listStatusFalse(userType,user,sort,sortColumn,pageNumber,pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/code",consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<UserTypeDTO> findByCode(
            @RequestParam("code") Long code
    ) throws BadRequestExceptions {
        UserTypeDTO result = iUserType.findByCode(code);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
}
