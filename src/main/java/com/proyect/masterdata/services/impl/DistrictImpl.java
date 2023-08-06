package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.District;
import com.proyect.masterdata.domain.DistrictPK;
import com.proyect.masterdata.dto.DistrictDTO;
import com.proyect.masterdata.dto.request.RequestDistrict;
import com.proyect.masterdata.dto.request.RequestDistrictSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.mapper.DistrictMapper;
import com.proyect.masterdata.repository.DistrictRepository;
import com.proyect.masterdata.services.IDistrict;
import com.proyect.masterdata.utils.Constants;
import lombok.AllArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@AllArgsConstructor
@Log4j2
public class DistrictImpl implements IDistrict {
    
    private final DistrictRepository districtRepository;
    private final DistrictMapper districtMapper;

    @Override
    public ResponseSuccess save(String name, String user, Long codeProvince) throws BadRequestExceptions {
        try {
            districtRepository.save(districtMapper.districtToName(RequestDistrictSave.builder()
                    .codeProvince(codeProvince)
                    .name(name.toUpperCase())
                    .user(user.toUpperCase()).build()));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileRegistering);
        }
    }

    @Override
    public ResponseSuccess saveAll(List<String> names, String user, Long codeProvince) throws BadRequestExceptions {
        try {
            List<RequestDistrictSave> districtSaves = names.stream().map(data -> RequestDistrictSave.builder()
                    .user(user.toUpperCase())
                    .codeProvince(codeProvince)
                    .name(data.toUpperCase())
                    .build()).toList();
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileRegistering);
        }
    }

    @Override
    public DistrictDTO update(RequestDistrict requestDistrict) throws BadRequestExceptions {
        try {
            requestDistrict.setName(requestDistrict.getName().toUpperCase());
            requestDistrict.setUser(requestDistrict.getUser().toUpperCase());
            District district = districtRepository.save(districtMapper.requestDistrictToDistrict(requestDistrict));
            return districtMapper.districtToDistrictDTO(district);

        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileUpdating);
        }
    }

    @Override
    public ResponseDelete delete(Long code, String user) throws BadRequestExceptions {
        try {
            districtRepository.deleteById(DistrictPK.builder()
                    .id(code)
                    .user(user)
                    .build());
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhenDeleting);
        }
    }

    @Override
    public ResponseDelete deleteAll(List<Long> codes, String user) throws BadRequestExceptions {
        try {
            List<DistrictPK> districtPKList = codes.stream().map(data -> DistrictPK.builder()
                    .user(user.toUpperCase())
                    .id(data)
                    .build()).toList();
            districtRepository.deleteAllById(districtPKList);
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhenDeleting);
        }
    }

    @Override
    public List<DistrictDTO> list() throws BadRequestExceptions {
        try {
            return districtMapper.listDistrictToListDistrictDTO(districtRepository.findAllByStatusTrue());
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public List<DistrictDTO> listStatusFalse() throws BadRequestExceptions {
        try {
            return districtMapper.listDistrictToListDistrictDTO(districtRepository.findAllByStatusFalse());
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public DistrictDTO findByCode(Long code) throws BadRequestExceptions {
        try {
            return districtMapper.districtToDistrictDTO(districtRepository.findByIdAndStatusTrue(code));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public DistrictDTO findByName(String name) throws BadRequestExceptions {
        try {
            return districtMapper.districtToDistrictDTO(districtRepository.findByNameAndStatusTrue(name.toUpperCase()));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public List<DistrictDTO> findByUser(String user) throws BadRequestExceptions {
        try {
            return districtMapper.listDistrictToListDistrictDTO(districtRepository.findByLoginUser(user.toUpperCase()));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public List<DistrictDTO> findAllProvinceId(Long codeProvince) throws BadRequestExceptions {
        try {
            return districtMapper.listDistrictToListDistrictDTO(districtRepository.findAllByStatusTrueAndProvinceId(codeProvince));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public List<DistrictDTO> findAllProvinceName(String nameProvincet) throws BadRequestExceptions {
        try {
            return districtMapper.listDistrictToListDistrictDTO(districtRepository.findAllByStatusTrueAndProvinceName(nameProvincet));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }
}
