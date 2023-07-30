package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Size;
import com.proyect.masterdata.domain.SizeType;
import com.proyect.masterdata.dto.MasterListDTO;
import com.proyect.masterdata.dto.response.ResponseMasterList;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.mapper.SizeTypeMapper;
import com.proyect.masterdata.repository.SizeTypeRepository;
import com.proyect.masterdata.services.IMasterList;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.sql.Date;
import java.util.List;

@Service
@RequiredArgsConstructor
public class SizeTypeImpl implements IMasterList {
    private final SizeTypeRepository sizeTypeRepository;
    private final SizeTypeMapper sizeTypeMapper;

    @Override
    public List<MasterListDTO> listRecords() throws BadRequestExceptions {
        return sizeTypeMapper.sizeTypeListToSizeTypeListDTO(sizeTypeRepository.findAll());
    }

    @Override
    public ResponseMasterList addRecord(String name) throws BadRequestExceptions {
        try{
            sizeTypeRepository.save(SizeType.builder().name(name).status(true).build());
            return ResponseMasterList.builder()
                    .code(200)
                    .message("Success")
                    .build();
        }catch(RuntimeException ex){
            throw new BadRequestExceptions(ex.getMessage());
        }
    }

    @Override
    public ResponseMasterList deleteRecord(Long id) throws BadRequestExceptions {
        try{
            SizeType sizeType = sizeTypeRepository.findById(id).get();
            sizeTypeRepository.save(SizeType.builder()
                    .name(sizeType.getName())
                    .dateRegistration(new Date(System.currentTimeMillis()))
                    .id(sizeType.getId())
                    .status(false)
                    .build());
            return ResponseMasterList.builder()
                    .code(200)
                    .message("Success")
                    .build();
        }catch (RuntimeException ex){
            throw new BadRequestExceptions(ex.getMessage());
        }
    }

    @Override
    public MasterListDTO updateRecord(String name, Long id) throws BadRequestExceptions {
        try{
            SizeType sizeType = sizeTypeRepository.save(SizeType.builder()
                    .id(id)
                    .dateRegistration(new Date(System.currentTimeMillis()))
                    .name(name)
                    .status(true)
                    .build()
            );
            return sizeTypeMapper.INSTANCE.sizeTypeToSizeTypeDTO(sizeType);
        }catch (RuntimeException ex){
            throw new BadRequestExceptions(ex.getMessage());
        }
    }
}
