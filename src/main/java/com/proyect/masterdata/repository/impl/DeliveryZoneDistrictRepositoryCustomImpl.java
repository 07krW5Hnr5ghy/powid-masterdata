package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.DeliveryZone;
import com.proyect.masterdata.domain.DeliveryZoneDistrict;
import com.proyect.masterdata.domain.District;
import com.proyect.masterdata.repository.DeliveryZoneDistrictRepositoryCustom;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.*;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.List;

@Repository
public class DeliveryZoneDistrictRepositoryCustomImpl implements DeliveryZoneDistrictRepositoryCustom {
    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;
    @Override
    public Page<DeliveryZoneDistrict> searchForDeliveryZoneDistrict(
            String deliveryZone,
            String district,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<DeliveryZoneDistrict> criteriaQuery = criteriaBuilder.createQuery(DeliveryZoneDistrict.class);
        Root<DeliveryZoneDistrict> itemRoot = criteriaQuery.from(DeliveryZoneDistrict.class);
        Join<DeliveryZoneDistrict, DeliveryZone> deliveryZoneDistrictDeliveryZoneJoin = itemRoot.join("deliveryZone");
        Join<DeliveryZoneDistrict, District> deliveryZoneDistrictDistrictJoin = itemRoot.join("district");

        criteriaQuery.select(itemRoot);
        List<Predicate> conditions = predicateConditions(
                deliveryZone,
                district,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                status,
                criteriaBuilder,
                itemRoot,
                deliveryZoneDistrictDeliveryZoneJoin,
                deliveryZoneDistrictDistrictJoin);

        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {
            List<Order> deliveryZoneDistrictList = new ArrayList<>();
            if (sort.equalsIgnoreCase("ASC")) {
                deliveryZoneDistrictList = listASC(sortColumn, criteriaBuilder, itemRoot,deliveryZoneDistrictDeliveryZoneJoin,deliveryZoneDistrictDistrictJoin);
            }
            if (sort.equalsIgnoreCase("DESC")) {
                deliveryZoneDistrictList = listDESC(sortColumn, criteriaBuilder, itemRoot,deliveryZoneDistrictDeliveryZoneJoin,deliveryZoneDistrictDistrictJoin);
            }
            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(deliveryZoneDistrictList);
        } else {
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }

        TypedQuery<DeliveryZoneDistrict> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber * pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        long count = getOrderCount(
                deliveryZone,
                district,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                status);
        return new PageImpl<>(orderTypedQuery.getResultList(), pageable, count);
    }
    public List<Predicate> predicateConditions(
            String deliveryZone,
            String district,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            Boolean status,
            CriteriaBuilder criteriaBuilder,
            Root<DeliveryZoneDistrict> itemRoot,
            Join<DeliveryZoneDistrict, DeliveryZone> deliveryZoneDistrictDeliveryZoneJoin,
            Join<DeliveryZoneDistrict, District> deliveryZoneDistrictDistrictJoin) {
        List<Predicate> conditions = new ArrayList<>();

        if(deliveryZone != null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(deliveryZoneDistrictDeliveryZoneJoin.get("name")),"%"+deliveryZone.toUpperCase()+"%"));
        }

        if(district != null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(deliveryZoneDistrictDistrictJoin.get("name")),"%"+district.toUpperCase()+"%"));
        }

        if(registrationStartDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.greaterThanOrEqualTo(itemRoot.get("registrationDate"),registrationStartDate)
                    )
            );
        }

        if(registrationEndDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.lessThanOrEqualTo(itemRoot.get("registrationDate"),registrationEndDate)
                    )
            );
        }

        if(updateStartDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.greaterThanOrEqualTo(itemRoot.get("updateDate"),updateStartDate)
                    )
            );
        }

        if(updateEndDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.lessThanOrEqualTo(itemRoot.get("updateDate"),updateEndDate)
                    )
            );
        }

        if(Boolean.TRUE.equals(status)) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isTrue(itemRoot.get("status"))));
        }

        if(Boolean.FALSE.equals(status)) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isFalse(itemRoot.get("status"))));
        }

        return conditions;
    }

    List<Order> listASC(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<DeliveryZoneDistrict> itemRoot,
            Join<DeliveryZoneDistrict, DeliveryZone> deliveryZoneDistrictDeliveryZoneJoin,
            Join<DeliveryZoneDistrict, District> deliveryZoneDistrictDistrictJoin
            ) {
        List<Order> deliveryZoneDistrictList = new ArrayList<>();
        if (sortColumn.equalsIgnoreCase("deliveryZone")) {
            deliveryZoneDistrictList.add(criteriaBuilder.asc(deliveryZoneDistrictDeliveryZoneJoin.get("name")));
        }
        if (sortColumn.equalsIgnoreCase("district")) {
            deliveryZoneDistrictList.add(criteriaBuilder.asc(deliveryZoneDistrictDistrictJoin.get("name")));
        }
        if (sortColumn.equalsIgnoreCase("registrationStartDate")) {
            deliveryZoneDistrictList.add(criteriaBuilder.asc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("registrationEndDate")) {
            deliveryZoneDistrictList.add(criteriaBuilder.asc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateStartDate")) {
            deliveryZoneDistrictList.add(criteriaBuilder.asc(itemRoot.get("updateDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateEndDate")) {
            deliveryZoneDistrictList.add(criteriaBuilder.asc(itemRoot.get("updateDate")));
        }
        return deliveryZoneDistrictList;
    }

    List<Order> listDESC(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<DeliveryZoneDistrict> itemRoot,
            Join<DeliveryZoneDistrict, DeliveryZone> deliveryZoneDistrictDeliveryZoneJoin,
            Join<DeliveryZoneDistrict, District> deliveryZoneDistrictDistrictJoin) {
        List<Order> deliveryZoneDistrictList = new ArrayList<>();
        if (sortColumn.equalsIgnoreCase("deliveryZone")) {
            deliveryZoneDistrictList.add(criteriaBuilder.desc(deliveryZoneDistrictDeliveryZoneJoin.get("name")));
        }
        if (sortColumn.equalsIgnoreCase("district")) {
            deliveryZoneDistrictList.add(criteriaBuilder.desc(deliveryZoneDistrictDistrictJoin.get("name")));
        }
        if (sortColumn.equalsIgnoreCase("registrationStartDate")) {
            deliveryZoneDistrictList.add(criteriaBuilder.desc(itemRoot.get("registrationDate")));
        }
        if (sortColumn.equalsIgnoreCase("registrationEndDate")) {
            deliveryZoneDistrictList.add(criteriaBuilder.desc(itemRoot.get("registrationDate")));
        }
        if (sortColumn.equalsIgnoreCase("updateStartDate")) {
            deliveryZoneDistrictList.add(criteriaBuilder.desc(itemRoot.get("updateDate")));
        }
        if (sortColumn.equalsIgnoreCase("updateEndDate")) {
            deliveryZoneDistrictList.add(criteriaBuilder.desc(itemRoot.get("updateDate")));
        }
        return deliveryZoneDistrictList;
    }

    private long getOrderCount(
            String deliveryZone,
            String district,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            Boolean status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<DeliveryZoneDistrict> itemRoot = criteriaQuery.from(DeliveryZoneDistrict.class);
        Join<DeliveryZoneDistrict, DeliveryZone> deliveryZoneDistrictDeliveryZoneJoin = itemRoot.join("deliveryZone");
        Join<DeliveryZoneDistrict, District> deliveryZoneDistrictDistrictJoin = itemRoot.join("district");

        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicateConditions(
                deliveryZone,
                district,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                status,
                criteriaBuilder,
                itemRoot,
                deliveryZoneDistrictDeliveryZoneJoin,
                deliveryZoneDistrictDistrictJoin);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
