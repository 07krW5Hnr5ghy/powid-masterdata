package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.Customer;
import com.proyect.masterdata.domain.District;
import com.proyect.masterdata.domain.Ordering;
import com.proyect.masterdata.domain.Province;
import com.proyect.masterdata.repository.OrderingRepositoryCustom;
import io.micrometer.common.util.StringUtils;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.*;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;
import java.util.List;

@Repository
public class OrderingRepositoryCustomImpl implements OrderingRepositoryCustom {
    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;
    @Override
    public Page<Ordering> searchForOrdering(
            Long orderId,
            Long clientId,
            String seller,
            String customer,
            String customerPhone,
            String instagram,
            List<Long> departmentIds,
            List<Long> provinceIds,
            List<Long> districtIds,
            List<Long> saleChannelIds,
            Boolean receiptFlag,
            Boolean deliveryFlag,
            Long orderStateId,
            Long courierId,
            Long paymentStateId,
            Long paymentMethodId,
            Long managementTypeId,
            Long storeId,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) {

        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Ordering> criteriaQuery = criteriaBuilder.createQuery(Ordering.class);
        Root<Ordering> itemRoot = criteriaQuery.from(Ordering.class);
        Join<Ordering, Customer> orderingCustomerJoin = itemRoot.join("customer");
        Join<Customer, District> customerDistrictJoin = orderingCustomerJoin.join("district");
        Join<District, Province> districtProvinceJoin = customerDistrictJoin.join("province");

        criteriaQuery.select(itemRoot);
        List<Predicate> conditions = predicateConditions(
                orderId,
                clientId,
                seller,
                customer,
                customerPhone,
                instagram,
                departmentIds,
                provinceIds,
                districtIds,
                saleChannelIds,
                receiptFlag,
                deliveryFlag,
                orderStateId,
                courierId,
                paymentStateId,
                paymentMethodId,
                managementTypeId,
                storeId,
                criteriaBuilder,
                itemRoot,
                orderingCustomerJoin,
                customerDistrictJoin,
                districtProvinceJoin);
        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {

            List<Order> orderingList = new ArrayList<>();

            if (sort.equalsIgnoreCase("ASC")) {
                orderingList = listASC(sortColumn, criteriaBuilder, itemRoot);
            }

            if (sort.equalsIgnoreCase("DESC")) {
                orderingList = listDESC(sortColumn, criteriaBuilder, itemRoot);
            }

            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(orderingList);

        } else {
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }

        TypedQuery<Ordering> orderingTypedQuery = entityManager.createQuery(criteriaQuery);
        orderingTypedQuery.setFirstResult(pageNumber * pageSize);
        orderingTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber,pageSize);
        Long count = getOrderCount(
                orderId,
                clientId,
                seller,
                customer,
                customerPhone,
                instagram,
                departmentIds,
                provinceIds,
                districtIds,
                saleChannelIds,
                receiptFlag,
                deliveryFlag,
                orderStateId,
                courierId,
                paymentStateId,
                paymentMethodId,
                managementTypeId,
                storeId);
        return new PageImpl<>(orderingTypedQuery.getResultList(),pageable,count);
    }

    List<Predicate> predicateConditions(
            Long id,
            Long clientId,
            String seller,
            String customer,
            String customerPhone,
            String instagram,
            List<Long> departmentIds,
            List<Long> provinceIds,
            List<Long> districtIds,
            List<Long> saleChannelIds,
            Boolean receiptFlag,
            Boolean deliveryFlag,
            Long orderStateId,
            Long courierId,
            Long paymentStateId,
            Long paymentMethodId,
            Long managementTypeId,
            Long storeId,
            CriteriaBuilder criteriaBuilder,
            Root<Ordering> itemRoot,
            Join<Ordering,Customer> orderingCustomerJoin,
            Join<Customer,District> customerDistrictJoin,
            Join<District,Province> districtProvinceJoin){
        List<Predicate> conditions = new ArrayList<>();

        if(id != null){
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("id"),id)));
        }

        if(clientId != null){
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("clientId"),clientId)));
        }

        if(seller != null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(itemRoot.get("seller")),"%"+seller.toUpperCase()+"%"));
        }

        if(customer != null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(orderingCustomerJoin.get("name")),"%"+customer.toUpperCase()+"%"));
        }

        if(customerPhone != null){
            conditions.add(criteriaBuilder.like(orderingCustomerJoin.get("phone"),"%"+customerPhone+"%"));
        }

        if(instagram != null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(orderingCustomerJoin.get("instagram")),"%"+instagram.toUpperCase()+"%"));
        }

        if(!departmentIds.isEmpty()){
            conditions.add(criteriaBuilder.and(districtProvinceJoin.get("departmentId").in(departmentIds)));
        }

        if (!provinceIds.isEmpty()){
            conditions.add(criteriaBuilder.and(customerDistrictJoin.get("provinceId").in(provinceIds)));
        }

        if(!districtIds.isEmpty()){
            conditions.add(criteriaBuilder.and(orderingCustomerJoin.get("districtId").in(districtIds)));
        }

        if(!saleChannelIds.isEmpty()){
            conditions.add(criteriaBuilder.and(itemRoot.get("saleChannelId").in(saleChannelIds)));
        }

        if (receiptFlag != null) {
            conditions.add(criteriaBuilder.equal(itemRoot.get("receiptFlag"), receiptFlag));
        }

        if (deliveryFlag != null) {
            conditions.add(criteriaBuilder.equal(itemRoot.get("deliveryFlag"), deliveryFlag));
        }

        if(orderStateId != null){
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("orderStateId"),orderStateId)));
        }

        if(courierId != null){
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("courierId"),courierId)));
        }

        if(paymentStateId != null){
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("paymentStateId"),paymentStateId)));
        }

        if(paymentMethodId != null){
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("paymentMethodId"),paymentMethodId)));
        }

        if(managementTypeId != null){
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("managementTypeId"),managementTypeId)));
        }

        if(storeId != null){
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("storeId"),storeId)));
        }

        return conditions;
    }

    private List<Order> listASC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<Ordering> itemRoot){

        List<Order> orderingList = new ArrayList<>();

        if(sortColumn.equals("id")){
            orderingList.add(criteriaBuilder.asc(itemRoot.get("id")));
        }

        if(sortColumn.equals("clientId")){
            orderingList.add(criteriaBuilder.asc(itemRoot.get("clientId")));
        }

        if(sortColumn.equals("orderStateId")){
            orderingList.add(criteriaBuilder.asc(itemRoot.get("orderStateId")));
        }

        if(sortColumn.equals("courierId")){
            orderingList.add(criteriaBuilder.asc(itemRoot.get("courierId")));
        }

        if(sortColumn.equals("paymentStateId")){
            orderingList.add(criteriaBuilder.asc(itemRoot.get("paymentStateId")));
        }

        if(sortColumn.equals("paymentMethodId")){
            orderingList.add(criteriaBuilder.asc(itemRoot.get("paymentMethodId")));
        }

        if(sortColumn.equals("saleChannelId")){
            orderingList.add(criteriaBuilder.asc(itemRoot.get("saleChannelId")));
        }

        if(sortColumn.equals("managementTypeId")){
            orderingList.add(criteriaBuilder.asc(itemRoot.get("managementTypeId")));
        }

        if(sortColumn.equals("storeId")){
            orderingList.add(criteriaBuilder.asc(itemRoot.get("storeId")));
        }

        if(sortColumn.equalsIgnoreCase("updateDate")){
            orderingList.add(criteriaBuilder.asc(itemRoot.get(
                    "updateDate"
            )));
        }

        if(sortColumn.equalsIgnoreCase("registrationDate")){
            orderingList.add(criteriaBuilder.asc(itemRoot.get(
                    "registrationDate"
            )));
        }

        return orderingList;

    }

    private List<Order> listDESC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<Ordering> itemRoot){

        List<Order> orderingList = new ArrayList<>();

        if(sortColumn.equals("id")){
            orderingList.add(criteriaBuilder.desc(itemRoot.get("id")));
        }

        if(sortColumn.equals("clientId")){
            orderingList.add(criteriaBuilder.desc(itemRoot.get("clientId")));
        }

        if(sortColumn.equals("orderStateId")){
            orderingList.add(criteriaBuilder.desc(itemRoot.get("orderStateId")));
        }

        if(sortColumn.equals("courierId")){
            orderingList.add(criteriaBuilder.desc(itemRoot.get("courierId")));
        }

        if(sortColumn.equals("paymentStateId")){
            orderingList.add(criteriaBuilder.desc(itemRoot.get("paymentStateId")));
        }

        if(sortColumn.equals("paymentMethodId")){
            orderingList.add(criteriaBuilder.desc(itemRoot.get("paymentMethodId")));
        }

        if(sortColumn.equals("saleChannelId")){
            orderingList.add(criteriaBuilder.desc(itemRoot.get("saleChannelId")));
        }

        if(sortColumn.equals("managementTypeId")){
            orderingList.add(criteriaBuilder.desc(itemRoot.get("managementTypeId")));
        }

        if(sortColumn.equals("storeId")){
            orderingList.add(criteriaBuilder.desc(itemRoot.get("storeId")));
        }

        if(sortColumn.equalsIgnoreCase("updateDate")){
            orderingList.add(criteriaBuilder.desc(itemRoot.get(
                    "updateDate"
            )));
        }

        if(sortColumn.equalsIgnoreCase("registrationDate")){
            orderingList.add(criteriaBuilder.desc(itemRoot.get(
                    "registrationDate"
            )));
        }

        return orderingList;

    }

    private Long getOrderCount(
            Long id,
            Long clientId,
            String seller,
            String customer,
            String customerPhone,
            String instagram,
            List<Long> departmentIds,
            List<Long> provinceIds,
            List<Long> districtIds,
            List<Long> saleChannelIds,
            Boolean receiptFlag,
            Boolean deliveryFlag,
            Long orderStateId,
            Long courierId,
            Long paymentStateId,
            Long paymentMethodId,
            Long managementTypeId,
            Long storeId){
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<Ordering> itemRoot = criteriaQuery.from(Ordering.class);
        Join<Ordering,Customer> orderingCustomerJoin = itemRoot.join("customer");
        Join<Customer, District> customerDistrictJoin = orderingCustomerJoin.join("district");
        Join<District, Province> districtProvinceJoin = customerDistrictJoin.join("province");

        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicateConditions(
                id,
                clientId,
                seller,
                customer,
                customerPhone,
                instagram,
                departmentIds,
                provinceIds,
                districtIds,
                saleChannelIds,
                receiptFlag,
                deliveryFlag,
                orderStateId,
                courierId,
                paymentStateId,
                paymentMethodId,
                managementTypeId,
                storeId,
                criteriaBuilder,
                itemRoot,
                orderingCustomerJoin,
                customerDistrictJoin,
                districtProvinceJoin);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
